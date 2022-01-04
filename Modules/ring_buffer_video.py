# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 12/21/21

# !/usr/bin/env python3

import time
import signal
from datetime import datetime
import sys
import csv
import RPi.GPIO as GPIO
import logging
from helper import logger_setup
from helper import csv_writer
from helper import box_id
from time import sleep
from os import walk
from picamera import PiCamera
from picamera import PiCameraCircularIO
from subprocess import call

logger_setup('/home/pi/')

REC_LED = 12
VIDEO_PIN = 13
GPIO.setmode(GPIO.BCM)
GPIO.setwarnings(False)
GPIO.setup(REC_LED, GPIO.OUT) 
    
video_duration = 10
video_data = "/home/pi/Data_ParentalCareTracking/Video/"
box_id = 101
    
# TODO try recording with ring buffer:
# See https://gist.github.com/waveform80/8496879
# Also see picamera docs section 3.12 Recording to circular stream in
# https://picamerax.readthedocs.io/en/latest/recipes1.html
def trigger_record_video(path, box_id, duration):
    GPIO.output(REC_LED, GPIO.HIGH)
    dt = datetime.now()
    dt_str = str(f"{dt.year}_{dt.month}_{dt.day}_{dt:%H}_{dt:%M}_{dt:%S}")
    file1_h264 = path + str(box_id) + 'before' + dt_str + '.h264'
    file2_h264 = path + str(box_id) + dt_str + '.h264'
    logging.info("Starting video recording...")
    #camera.start_recording(file_h264)
    
    # Record frames after trigger
    camera.split_recording(file2_h264)
    camera.wait_recording(duration)
    
    # Write frames before trigger as well
    stream.copy_to(file1_h264, seconds = 10)
    stream.clear()
    
    # Split recording back to the circular buffer in memory
    camera.wait_recording(1)
    camera.split_recording(stream)
    camera.stop_recording()
    GPIO.output(REC_LED, GPIO.LOW)
    logging.info("Video recorded")
    camera.close()
    
    file1_mp4 = path + str(box_id) + 'before' + dt_str + '.mp4'
    file2_mp4 = path + str(box_id) + dt_str + '.mp4'
    command1 = "MP4Box -add " + file1_h264 + " " + file1_mp4
    command2 = "MP4Box -add " + file2_h264 + " " + file2_mp4
    call([command1], shell=True)
    call([command2], shell=True)
    logging.info('Converted video ' + file1_h264 + ' to mp4.')
    logging.info('Converted video ' + file2_h264 + ' to mp4.')
    
    # Will have to combine files afterwards with ffmpeg??
    

#def convert(file_h264):
#    command = "MP4Box -add " + file_h264 + " " + file_mp4
#    call([command], shell=True)
#    logging.info('Converted video h264 to mp4.')

camera.resolution = (1280, 720)
stream = PiCameraCircularIO(camera, seconds=10)
camera.start_recording(stream, format='h264')

try:
    while True:
        #pass
        camera.wait_recording(1)
        if GPIO.input(VIDEO_PIN):
            trigger_record_video(video_data, box_id, video_duration)

except KeyboardInterrupt:
    logging.info('exiting IRBB')
    GPIO.output(REC_LED, GPIO.LOW)
    GPIO.cleanup()

finally:
    GPIO.output(REC_LED, GPIO.LOW)
    GPIO.cleanup()
        
