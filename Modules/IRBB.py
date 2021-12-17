# Created by PyCharm
# Author: nmoltta, gsvidaurre
# Project: ParentalCareTracking
# Date: 11/16/2021

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
from picamera import PiCamera
from subprocess import call
from os import walk
import threading

logger_setup('/home/pi/')

# GPIO pin IDs through which IR receivers transmit data
# BEAM_PIN = 16
BEAM_PIN_lead = 16
BEAM_PIN_rear = 19

warn = 0
module = 'IRBB'

# CSV header
header = ['chamber_id', 'sensor_id', 'year', 'month', 'day', 'timestamp']
# sensor_id = "rear"
irbb_data = "/home/pi/Data_ParentalCareTracking/IRBB/"
video_data = "/home/pi/Data_ParentalCareTracking/Video/"

logging.info('started irbb script')


def convert(file_h264):
    command = "MP4Box -add " + file_h264 + " " + file_mp4
    call([command], shell=True)
    logging.info('Converted video h264 to mp4.')


# TODO try recording with ring buffer:
# See https://gist.github.com/waveform80/8496879
# Also see picamera docs section 3.12 Recording to circular stream in
# https://picamerax.readthedocs.io/en/latest/recipes1.html
def record_video(path, box_id, dt):
    dt_str = str(f"{dt.year}_{dt.month}_{dt.day}_{dt:%H}_{dt:%M}_{dt:%S}")
    file_h264 = path + str(box_id) + dt_str + '.h264'
    exists = false
    # test if threading doesn't work:
    # prev = dt - datetime.timedelta(seconds=60)
    # prev_str = str(f"{prev.year}_{prev.month}_{prev.day}_{prev:%H}_{prev:%M}_{prev:%S}")

    # for (dirpath, dirnames, filenames) in walk(path):
    #     if filenames == path + str(box_id) + prev_str + '.h264':
    #         exists = true
    #         break

    if not exists:
        camera = PiCamera()
        # Need to play as well since the light in the box will be artificial, probably will use 800 all the time
        camera.iso = 400
        # 1280x720 resolution is a 16:9 wide-screen aspect, looks pixelated
        # Anything x 1080 throws an error, 1024x768 is a 4:3 aspect ratio
        # Will need to play around with this to set width and height
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


# Need to update recording function to record only if the previous video
# was recorded 90 seconds or some threshold ago
def detect_beam_breaks_callback(BEAM_PIN, sensor_id):
    if not GPIO.input(BEAM_PIN):
        dt = datetime.now()
        logging.info('IRBB activity detected.')
        csv_writer(str(box_id), 'IRBB', irbb_data, f"{dt.year}_{dt.month}_{dt.day}",
                   header, [box_id, sensor_id, f"{dt.year}", f"{dt.month}", f"{dt.day}", f"{dt:%H:%M:%S.%f}"])
        if sensor_id == "rear":
            video = threading.Thread(target=record_video(video_data, box_id, dt), args=(), kwargs={})
            if not video.is_alive():
                video.start()
            # else:
            #     video.join()  # redundant maybe? it will wait until video ends
        # sleep(1)  # no longer needed?


# Handler function for manual Ctrl + C cancellation
def signal_handler(sig, frame):
    GPIO.cleanup()
    sys.exit(0)


GPIO.setmode(GPIO.BCM)
GPIO.setup(BEAM_PIN_lead, GPIO.IN, pull_up_down=GPIO.PUD_UP)
GPIO.setup(BEAM_PIN_rear, GPIO.IN, pull_up_down=GPIO.PUD_UP)

GPIO.add_event_detect(BEAM_PIN_lead, GPIO.FALLING,
                      callback=lambda x: detect_beam_breaks_callback(BEAM_PIN_lead, "lead"), bouncetime=100)
GPIO.add_event_detect(BEAM_PIN_rear, GPIO.FALLING,
                      callback=lambda x: detect_beam_breaks_callback(BEAM_PIN_rear, "rear"), bouncetime=100)

try:
    while True:
        pass

except KeyboardInterrupt:
    logging.info('exiting IRBB')
    GPIO.cleanup()

finally:
    GPIO.cleanup()
