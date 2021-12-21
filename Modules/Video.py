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

REC_LED = 12  # change accordingly but same as IRBB

warn = 0
module = 'Video'
video_data = "/home/pi/Data_ParentalCareTracking/Video/"
GPIO.setmode(GPIO.BCM)
GPIO.setup(REC_LED, GPIO.IN, pull_up_down=GPIO.PUD_DOWN)


def convert(file_h264):
    command = "MP4Box -add " + file_h264 + " " + file_mp4
    call([command], shell=True)
    logging.info('Converted video h264 to mp4.')


# TODO try recording with ring buffer:
# See https://gist.github.com/waveform80/8496879
# Also see picamera docs section 3.12 Recording to circular stream in
# https://picamerax.readthedocs.io/en/latest/recipes1.html
def record_video(path, box_id):
    dt = datetime.now()
    dt_str = str(f"{dt.year}_{dt.month}_{dt.day}_{dt:%H}_{dt:%M}_{dt:%S}")
    file_h264 = path + str(box_id) + dt_str + '.h264'
    camera = PiCamera()
    # Need to play with these settings later
    camera.iso = 400
    camera.resolution = (640, 640)
    camera.framerate = 60
    camera.start_recording(file_h264)
    camera.wait_recording(60)
    camera.stop_recording()
    camera.close()
    file_mp4 = path + str(box_id) + dt_str + '.mp4'
    # MP4Box prints a lot of messages, could these be saved to the log via Main.sh?
    command = "MP4Box -add " + file_h264 + " " + file_mp4
    call([command], shell=True)
    logging.info('Converted video ' + file_h264 + ' to mp4.')


try:
    while True:
        if GPIO.input(int(REC_LED)):
            record_video(video_data, box_id)
            pass

except KeyboardInterrupt:
    logging.info('exiting Video')
    GPIO.cleanup()

finally:
    GPIO.cleanup()
