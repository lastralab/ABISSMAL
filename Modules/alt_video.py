# Created by PyCharm
# Author: nmoltta, gsvidaurre
# Project: Abissmal
# Date: 12/21/21

# !/usr/bin/env python3

import os
import time
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
from helper import sms_alert
from helper import get_logger

dir_setup('/home/pi/')
logging = get_logger(datetime.today())
logging.info('Starting Validation Video script')
print('Started Validation Video script')

path = "/home/pi/Data_Abissmal/Video/"
header = ['chamber_id', 'sensor_id', 'year', 'month', 'day', 'time_video_started', 'video_file_name']
prior_image = None
video_time_range = [7, 12]
video_width = 1280
video_height = 720
iso = 400
fr = 30
record_duration = 5
REC_LED = 16
LED_time_range = [7, 20]
GPIO.setmode(GPIO.BCM)
GPIO.setwarnings(False)
GPIO.setup(REC_LED, GPIO.OUT)
GPIO.PWM(REC_LED, 40)

logging.info('Validation videos will be recorded between ' + str(video_time_range[0]) + ' and ' + str(video_time_range[1] + 1) + ' hours')
print('Validation videos will be recorded between ' + str(video_time_range[0]) + ' and ' + str(video_time_range[1] + 1) + ' hours')


def set_prior_image(current_image):
    global prior_image
    prior_image = current_image


def convert_video(filename):
    try:
        file_mp4 = path + Path(filename).stem + '.mp4'
        command = "MP4Box -add " + filename + " " + file_mp4
        call([command], shell=True)
        print('Converted video')
        os.remove(filename)
        csv_writer(str(box_id), 'Validation', path, f"{dt.year}_{dt.month}_{dt.day}",
                   header,
                   [box_id, 'Camera', f"{dt.year}", f"{dt.month}", f"{dt.day}", f"{dt:%H:%M:%S.%f}",
                    Path(filename).stem + '.mp4'])
    except Exception as Err:
        logging.error('Converting validation video error: ' + str(Err))
        sms_alert('Validation', 'Conversion Error: ' + str(Err))


with picamera.PiCamera() as camera:
    try:
        camera.resolution = (video_width, video_height)
        camera.iso = iso
        camera.framerate = fr
        while True:
            general_time = datetime.now()
            logging = get_logger(general_time)
            hour_int = int(f"{general_time:%H}")
            if int(video_time_range[0]) <= hour_int < int(video_time_range[1]):
                dt = general_time
                print('Validation recording started')
                dt_str = str(f"{dt.year}_{dt.month}_{dt.day}_{dt:%H}_{dt:%M}_{dt:%S}")
                file1_h264 = path + str(box_id) + "_validation_" + dt_str + '.h264'
                if int(LED_time_range[0]) <= hour_int < int(LED_time_range[1]):
                    GPIO.output(REC_LED, GPIO.HIGH)
                camera.start_recording(file1_h264)
                camera.wait_recording(record_duration)
                camera.stop_recording()
                print('Recorded video starting at ' + "{dt:%H}:{dt:%M}:{dt:%S}")
                if int(LED_time_range[0]) <= hour_int <= int(LED_time_range[1]):
                    GPIO.output(REC_LED, GPIO.LOW)
                convert_video(file1_h264)
                print('Converted video to mp4')
                logging.info("Recorded and converted video to mp4")
    except Exception as E:
        print('Video error: ' + str(E))
        logging.error('Video: ' + str(E))
        sms_alert('Video', 'Error: ' + str(E))
        camera.close()
        logging.info("Camera closed")
    finally:
        camera.stop_recording()
        camera.close()
        GPIO.output(REC_LED, GPIO.LOW)
        logging.info("Camera closed")
