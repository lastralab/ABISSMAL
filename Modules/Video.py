# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 12/21/21

# !/usr/bin/env python3

# Should we be saving an entry to a .csv file every time a video is recorded??
# To avoid RPi freezing always cancel with Ctrl + C, not "Stop"

import time
import signal
from datetime import datetime
import sys
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
#from picamera import PiCamera
from PIL import Image
from subprocess import call
from pathlib import Path

logger_setup('/home/pi/')

# Code taken from picamera advanced recipes, image comparison from link below
# Check out "lightweight Python motion detection" https://forums.raspberrypi.com/viewtopic.php?t=45235&sid=b32ca9b34abf8243de9b15ddff22faf0

path = "/home/pi/Data_ParentalCareTracking/Video/"
prior_image = None

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
threshold = 10
sensitivity = 20

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
        
        # Compare current_image to prior_image to detect motion
        # Count changed pixels
        changedPixels = 0
        # xrange no longer exists in Python 3
        for x in range(0, 100):
            for y in range(0, 75):
                # Check green channel only (originally noted to be the "highest quality" channel)
                pixdiff = abs(buffer1[x,y][1] - buffer2[x,y][1])
                if pixdiff > threshold:
                    changedPixels += 1
                    
        # Trigger if sufficient pixels changed
        if changedPixels > sensitivity:
            result = True
        else:
            result = False
        
        # Once motion detection is done, make the prior image the current
        prior_image = current_image
        return result
    
    
def convert_video(filename):
    #file_mp4 = path + str(box_id) + dt_str + '.mp4'
    file_mp4 = Path(filename).stem + '.mp4'
    command = "MP4Box -add " + filename + " " + file_mp4
    call([command], shell=True)
    logging.info('Converted video ' + filename + ' to mp4.')


# TKTK need to update filenames so date time is distinct and a separate pair of videos is made per trigger
with picamera.PiCamera() as camera:
    dt = datetime.now()
    hour_int = int(f"{dt:%H}")
    
    if hour_int > 6 and hour_int < 19:
        camera.resolution = (video_width, video_height)
        camera.iso = iso
        camera.framerate = fr
        stream = picamera.PiCameraCircularIO(camera, seconds = stream_duration)
        camera.start_recording(stream, format = 'h264')
        
        dt_str = str(f"{dt.year}_{dt.month}_{dt.day}_{dt:%H}_{dt:%M}_{dt:%S}")
        file1_h264 = path + str(box_id) + "_pre-trigger_" + dt_str + '.h264'
        file2_h264 = path + str(box_id) + dt_str + '.h264'
         
        
        try:
            while True:
                camera.wait_recording(1)
                if detect_motion(camera):
                    print('Motion detected; Recording started')
                    logging.info("Starting video recording...")
                    GPIO.output(REC_LED, GPIO.HIGH)
                    # As soon as we detect motion, split the recording to
                    # record the frames "after" motion
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
                    
                    # TKTK need to add video conversions
                    
        finally:
            camera.stop_recording()
            camera.close()
            GPIO.output(REC_LED, GPIO.LOW)
            

# Convert videos to mp4 as soon as recording hours are over
# Instead, Tania will write code to convert videos before backups too
#try:
#    while True:
        
##        time_only = datetime.now().time()
#        hour_only = int(f"{time_only:%H}")
        
#        if(hour_only > 19):
#            convert_video(file1_h264)
#            convert_video(file2_h264)
#        else:
#            pass