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
import io
import random
import picamera
from helper import dir_setup
from helper import csv_writer
from helper import box_id
from time import sleep
from os import walk
from PIL import Image
from subprocess import call
from pathlib import Path
from helper import email_alert
from helper import get_logger

dir_setup('/home/pi/')
logging = get_logger(datetime.today())
logging.info('Starting Video script')
print('Starting Video script')

path = "/home/pi/Data_ParentalCareTracking/Video/"
header = ['chamber_id', 'year', 'month', 'day', 'time_video_started', 'video_file_name']
prior_image = None
time_range = [6, 19]
video_width = 1280
video_height = 720
iso = 800
fr = 30
stream_duration = 5
record_duration = 10
threshold = 50
sensitivity = 80
REC_LED = 16
GPIO.setmode(GPIO.BCM)
GPIO.setwarnings(False)
GPIO.setup(REC_LED, GPIO.OUT)
GPIO.output(REC_LED, GPIO.LOW)

logging.info('Video will be recorded between ' + str(time_range[0]) + ' and ' + str(time_range[1]) + ' hours')
print('Video will be recorded between ' + str(time_range[0]) + ' and ' + str(time_range[1]) + ' hours')


def detect_motion(cam):
    global prior_image
    streaming = io.BytesIO()
    cam.capture(streaming, format='jpeg', use_video_port=True)
    streaming.seek(0)
    if prior_image is None:
        prior_image = Image.open(streaming)
        return False
    else:
        buffer1 = prior_image.load()
        current_image = Image.open(streaming)
        buffer2 = current_image.load()
        pixels = 0
        for x in range(0, video_width):
            for y in range(0, video_height):
                pixdiff1 = abs(buffer1[x, y][0] - buffer2[x, y][0])
                pixdiff2 = abs(buffer1[x, y][1] - buffer2[x, y][1])
                pixdiff3 = abs(buffer1[x, y][2] - buffer2[x, y][2])
                if (pixdiff1 + pixdiff2 + pixdiff3) > (threshold * 3):
                    pixels += 1
        if pixels > sensitivity:
            result = True
        else:
            result = False
        prior_image = current_image
        return result


def convert_video(filename):
    try:
        file_mp4 = path + Path(filename).stem + '.mp4'
        command = "MP4Box -add " + filename + " " + file_mp4
        call([command], shell=True)
        print('Converted video')
        os.remove(filename)
        csv_writer(str(box_id), 'Video', path, f"{dt.year}_{dt.month}_{dt.day}",
                   header,
                   [box_id, f"{dt.year}", f"{dt.month}", f"{dt.day}", f"{dt:%H:%M:%S.%f}", Path(filename).stem + '.mp4'])
    except Exception as Err:
        logging.error('Converting video error: ' + str(Err))
        email_alert('Video', 'Convert Error: ' + str(Err))


with picamera.PiCamera() as camera:
    try:
        camera.resolution = (video_width, video_height)
        camera.iso = iso
        camera.framerate = fr
        stream = picamera.PiCameraCircularIO(camera, seconds=stream_duration)
        camera.start_recording(stream, format='h264')
        while True:
            general_time = datetime.now()
            hour_int = int(f"{general_time:%H}")
            if int(time_range[0]) <= hour_int <= int(time_range[1]):
                if detect_motion(camera):
                    print('Motion detected; Recording started')
                    logging.info("Motion detected. Starting video recordings")
                    GPIO.output(REC_LED, GPIO.HIGH)
                    dt = datetime.now()
                    dt_str = str(f"{dt.year}_{dt.month}_{dt.day}_{dt:%H}_{dt:%M}_{dt:%S}")
                    file1_h264 = path + str(box_id) + "_" + dt_str + "_pre_trigger" + '.h264'
                    file2_h264 = path + str(box_id) + "_" + dt_str + "_post_trigger" + '.h264'
                    camera.split_recording(file2_h264)
                    camera.wait_recording(record_duration)
                    stream.copy_to(file1_h264, seconds=stream_duration)
                    stream.clear()
                    print('Recording finished')
                    logging.info("Videos recorded")
                    GPIO.output(REC_LED, GPIO.LOW)
                    camera.wait_recording(1)
                    camera.split_recording(stream)
                    convert_video(file1_h264)
                    convert_video(file2_h264)
                    print('Converted videos to mp4')
                    logging.info("Converted videos to mp4")
    except Exception as E:
        print('Video error: ' + str(E))
        logging.error('Video: ' + str(E))
        email_alert('Video', 'Error: ' + str(E))
    finally:
        camera.stop_recording()
        camera.close()
        GPIO.output(REC_LED, GPIO.LOW)
