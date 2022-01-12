# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 12/21/21

# !/usr/bin/env python3

# Should we be saving an entry to a .csv file every time a video is recorded??
# To avoid RPi freezing always cancel with Ctrl + C, not "Stop"

import os
import time
import signal
from datetime import datetime
import sys
import csv
import re
import RPi.GPIO as GPIO
import logging
import io
import random
import picamera
from helper import logger_setup
from helper import csv_writer
from helper import box_id
from time import sleep
from os import walk
from PIL import Image
from subprocess import call
from pathlib import Path
import numpy as np

logger_setup('/home/pi/')

# Code taken from picamera advanced recipes, image comparison from link below
# Check out "lightweight Python motion detection" https://forums.raspberrypi.com/viewtopic.php?t=45235&sid=b32ca9b34abf8243de9b15ddff22faf0

path = "/home/pi/Data_ParentalCareTracking/Video/"
header = ['chamber_id', 'year', 'month', 'day', 'time_video_started', 'video_file_name']
prior_image = None

# Time range (military) during which video will be recorded
time_range = np.array([6, 20])

# Video settings
video_width = 640
video_height = 480
iso = 400
fr = 60

# Stream (ring buffer) and recording settings
stream_duration = 5
record_duration = 10

# Motion detection settings:
# Threshold (how much a pixel has to change by to be marked as "changed")
# Sensitivity (how many changed pixels before capturing an image)
threshold = 50 # threshold of 10 picks up finer-scale movement (light table shaking, quick pass of hand)
sensitivity = 80 # value of 20 picks up finer scale motions (light table shaking, quick pass of hand)

REC_LED = 12
VIDEO_PIN = 13
GPIO.setmode(GPIO.BCM)
GPIO.setwarnings(False)
GPIO.setup(REC_LED, GPIO.OUT)
GPIO.output(REC_LED, GPIO.LOW)


def detect_motion(camera):
    global prior_image
    stream = io.BytesIO()
    camera.capture(stream, format='jpeg', use_video_port=True)
    stream.seek(0)
    if prior_image is None:
        prior_image = Image.open(stream)
        return False
    else:
        buffer1 = prior_image.load()
        
        current_image = Image.open(stream)
        buffer2 = current_image.load()
        
        # Count changed pixels to compare current_image to prior_image to detect motion
        changedPixels = 0
        
        # Loop over the pixels expected in the x and y dimensions
        # Detect changes in pixels in all 3 color channels
        for x in range(0, video_width):
            for y in range(0, video_height):
                pixdiff1 = abs(buffer1[x,y][0] - buffer2[x,y][0])
                # Green channel
                # Originally noted to be the "highest quality" channel, and was the only channel monitored
                pixdiff2 = abs(buffer1[x,y][1] - buffer2[x,y][1])
                pixdiff3 = abs(buffer1[x,y][2] - buffer2[x,y][2])
                if (pixdiff1 + pixdiff2 + pixdiff3) > (threshold * 3):
                    changedPixels += 1      
                    
        # Trigger if sufficient pixels changed
        if changedPixels > sensitivity:
            result = True
        else:
            result = False
        
        # Once motion detection is done, make the prior image the current
        prior_image = current_image
        return result
    
    
# TKTK getting ffmpeg error "101_2022_1_11_18_42_13_pre-trigger.mp4: Invalid data found when processing input"
# I also get a warning "WARNING: library configuration mismatch"
# ffmpeg version 4.1.6-1
# Might need to update ffmpeg

# 10 Jan 2021
# sudo apt purge --autoremove -y ffmpeg
# sudo apt update
# sudo apt install -y ffmpeg
# ffmpeg -version
# ffmpeg version 4.1.8-0+deb10u1+rpt1 Copyright (c) 2000-2021

# Combining .h264: [concat @ 0x1a3f280] Line 4: unknown keyword '?'d'
#/home/pi/Data_ParentalCareTracking/Video/101_2022_1_11_19_15_14_pre_trigger.h264: Invalid data found when processing input

def convert_video(filename):
    file_mp4 = path + Path(filename).stem + '.mp4'
    command = "MP4Box -add " + filename + " " + file_mp4
    call([command], shell = True)
    logging.info('Converted video ' + filename + ' to mp4.')
    #os.remove(filename)

def combine_video(file1, file2):
    #file1 = path + Path(file1).stem + ".mp4"
    #file2 = path + Path(file2).stem + ".mp4"
    out_file = re.sub(r"_post_trigger", "", file2)
    #out_file = Path(out_file).stem + ".mp4"
    in_files = file1 + " " + file2
    os.system(r"/bin/ffmpeg" + " -f concat -i " + in_files + " -safe 0 -c copy " + out_file)
    csv_writer(str(box_id), 'Video', path, f"{dt.year}_{dt.month}_{dt.day}",
               header, [box_id, f"{dt.year}", f"{dt.month}", f"{dt.day}", f"{dt:%H:%M:%S.%f}", out_file])
    #os.remove(file1)
    #os.remove(file2)
    
    convert_video(out_file)
             

with picamera.PiCamera() as camera:
    general_time = datetime.now()
    hour_int = int(f"{general_time:%H}")
    

    if hour_int > time_range[0] and hour_int < time_range[1]:
        camera.resolution = (video_width, video_height)
        camera.iso = iso
        camera.framerate = fr
        stream = picamera.PiCameraCircularIO(camera, seconds = stream_duration)
        camera.start_recording(stream, format = 'h264') 
        
        try:
            while True:
                camera.wait_recording(1)
                if detect_motion(camera):
                    print('Motion detected; Recording started')
                    logging.info("Starting video recording...")
                    GPIO.output(REC_LED, GPIO.HIGH)
                    # As soon as we detect motion, split the recording to
                    # record the frames "after" motion
                    dt = datetime.now()
                    dt_str = str(f"{dt.year}_{dt.month}_{dt.day}_{dt:%H}_{dt:%M}_{dt:%S}")
                    file1_h264 = path + str(box_id) + "_" + dt_str + "_pre_trigger" + '.h264'
                    file2_h264 = path + str(box_id) + "_" + dt_str + "_post_trigger" + '.h264'
                    camera.split_recording(file2_h264)
                    # Record for the specified duration of time after motion detection
                    camera.wait_recording(record_duration)
                    # Write the 10 seconds "before" motion to disk as well
                    stream.copy_to(file1_h264, seconds = stream_duration)
                    stream.clear()
                    print('Recording finished')
                    logging.info("Video recorded")
                    GPIO.output(REC_LED, GPIO.LOW)
                    
                    # Wait briefly, then split recording back to the in-memory circular buffer
                    camera.wait_recording(1)
                    camera.split_recording(stream)
                    
                    # Downside to conversion and combining here is that will miss movements that occur while files converting
                    #convert_video(file1_h264)
                    #convert_video(file2_h264)
                    
                    combine_video(file1_h264, file2_h264)
                                        
                    
        finally:
            camera.stop_recording()
            camera.close()
            GPIO.output(REC_LED, GPIO.LOW)
            