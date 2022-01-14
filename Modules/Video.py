# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 12/21/21

# !/usr/bin/env python3

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

path = "/home/pi/Data_ParentalCareTracking/Video/"
header = ['chamber_id', 'year', 'month', 'day', 'time_video_started', 'video_file_name']
prior_image = None

time_range = np.array([6, 19])

video_width = 640
video_height = 480
iso = 400
fr = 60

stream_duration = 5
record_duration = 10

threshold = 50
sensitivity = 80

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
        
        changedPixels = 0
        
        for x in range(0, video_width):
            for y in range(0, video_height):
                pixdiff1 = abs(buffer1[x,y][0] - buffer2[x,y][0])
                pixdiff2 = abs(buffer1[x,y][1] - buffer2[x,y][1])
                pixdiff3 = abs(buffer1[x,y][2] - buffer2[x,y][2])
                if (pixdiff1 + pixdiff2 + pixdiff3) > (threshold * 3):
                    changedPixels += 1      
                    
        if changedPixels > sensitivity:
            result = True
        else:
            result = False
        
        prior_image = current_image
        return result
    

def convert_video(filename):
    file_mp4 = path + Path(filename).stem + '.mp4'
    command = "MP4Box -add " + filename + " " + file_mp4
    call([command], shell = True)
    logging.info('Converted video ' + filename + ' to mp4.')
    os.remove(filename)
    csv_writer(str(box_id), 'Video', path, f"{dt.year}_{dt.month}_{dt.day}",
               header, [box_id, f"{dt.year}", f"{dt.month}", f"{dt.day}", f"{dt:%H:%M:%S.%f}", Path(filename).stem + '.mp4'])
             

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

                    dt = datetime.now()
                    dt_str = str(f"{dt.year}_{dt.month}_{dt.day}_{dt:%H}_{dt:%M}_{dt:%S}")
                    file1_h264 = path + str(box_id) + "_" + dt_str + "_pre_trigger" + '.h264'
                    file2_h264 = path + str(box_id) + "_" + dt_str + "_post_trigger" + '.h264'
                    camera.split_recording(file2_h264)

                    camera.wait_recording(record_duration)

                    stream.copy_to(file1_h264, seconds = stream_duration)
                    stream.clear()
                    print('Recording finished')
                    logging.info("Video recorded")
                    GPIO.output(REC_LED, GPIO.LOW)
                    
                    camera.wait_recording(1)
                    camera.split_recording(stream)
                    
                    convert_video(file1_h264)
                    convert_video(file2_h264)
                                        
                    
        finally:
            camera.stop_recording()
            camera.close()
            GPIO.output(REC_LED, GPIO.LOW)
            