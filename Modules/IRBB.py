# Created by PyCharm
# Author: nmoltta, gsvidaurre
# Project: ParentalCareTracking
# Date: 11/16/2021

#!/usr/bin/env python3

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

logger_setup('/home/pi/')

# GPIO pin IDs through which IR receivers transmit data
#BEAM_PIN = 16
BEAM_PIN_lead = 16
BEAM_PIN_rear = 19

warn = 0
module = 'IRBB'

# CSV header
header = ['chamber_id', 'sensor_id', 'year', 'month', 'day', 'timestamp']
#sensor_id = "rear"
irbb_data = "/home/pi/Data_ParentalCareTracking/IRBB"
video_data = "/home/pi/Data_ParentalCareTracking/Video"

logging.info('started irbb script')

def convert(file_h264):

    command = "MP4Box -add " + file_h264 + " " + file_mp4
    call([command], shell=True)
    logging.info('Converted video h264 to mp4.')


# TODO test adding function to beam breaker function, passing time param
# TODO try catch, log errors
# TODO try recording with ring buffer:
# See https://gist.github.com/waveform80/8496879
# Also see picamera docs section 3.12 Recording to circular stream in https://picamerax.readthedocs.io/en/latest/recipes1.html
def record_video(path, box_id, dt_str):
    file_h264 = path + box_id + dt_str + '.h264'
    camera = PiCamera()
    camera.resolution = (640, 480)
    camera.framerate = 60
    camera.start_preview()
    camera.start_recording(file_h264)
    sleep(60)
    camera.stop_recording()
    camera.stop_preview()
    file_mp4 = path + box_id + dt_str + '.mp4'
    command = "MP4Box -add " + file_h264 + " " + file_mp4
    call([command], shell=True)
    logging.info('Converted video ' + filename + ' to mp4.')

def detect_beam_breaks_callback(BEAM_PIN, sensor_id):
    if not GPIO.input(BEAM_PIN):
        dt = datetime.now()
        logging.info('IRBB activity detected at: ' + f"{dt:%H:%M:%S.%f}")
        # This finally prints a .csv but the callback isn't working
        csv_writer(str(box_id), 'IRBB', irbb_data, f"{dt.year}_{dt.month}_{dt.day}",
                   header, [box_id, sensor_id, f"{dt.year}", f"{dt.month}", f"{dt.day}", f"{dt:%H:%M:%S.%f}"])
        # Only record if the rear beam is broken
        # Sleep for 90 seconds to ignore subsequent triggers of rear sensor while video is recording
        if(sensor_id = "rear"):
            record_video(path, box_id, f"{dt.year}_{dt.month}_{dt.day}_{dt:%H:%M:%S.%z}")
            sleep(90)
        sleep(1)


 # Handler function for manual Ctrl + C cancellation
def signal_handler(sig, frame):
     GPIO.cleanup()
     sys.exit(0)


GPIO.setmode(GPIO.BCM)
GPIO.setup(BEAM_PIN_lead, GPIO.IN, pull_up_down=GPIO.PUD_UP)
GPIO.setup(BEAM_PIN_rear, GPIO.IN, pull_up_down=GPIO.PUD_UP)

GPIO.add_event_detect(BEAM_PIN_lead, GPIO.FALLING, callback=lambda x: detect_beam_breaks_callback(BEAM_PIN_lead, "lead"), bouncetime=100)
GPIO.add_event_detect(BEAM_PIN_rear, GPIO.FALLING, callback=lambda x: detect_beam_breaks_callback(BEAM_PIN_rear, "rear"), bouncetime=100)


try:
    while True:
        pass
    
except KeyboardInterrupt:
    logging.info('exiting IRBB')
    GPIO.cleanup()
    
finally:
    GPIO.cleanup()

