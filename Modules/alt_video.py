# Created by PyCharm
# Author: tanismo
# Project: Abissmal
# Date: 12/21/21

# !/usr/bin/env python3

import os
import time
from datetime import datetime
import sys
import csv
import re
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
video_time_range = [[7, 10]]
count = video_time_range.__len__()
pause = 0
video_width = 1280
video_height = 720
iso = 400
fr = 30
record_duration = 5

logging.info('Detected ' + str(count) + ' time slots')
print('Detected ' + str(count) + ' time slots')

for times in video_time_range:
    logging.info('Validation videos will be recorded between ' + str(times[0]) + ' and ' + str(times[1]) + ' hours')
    print('Validation videos will be recorded between ' + str(times[0]) + ' and ' + str(times[1]) + ' hours')


def convert_video(filename):
    try:
        file_mp4 = path + Path(filename).stem + '.mp4'
        command = "MP4Box -add " + filename + " " + file_mp4
        call([command], shell=True)
        print('Converted video')
        os.remove(filename)
        csv_writer(str(box_id), 'Video', path, f"{dt.year}_{dt.month}_{dt.day}",
                   header,
                   [box_id, 'Camera', f"{dt.year}", f"{dt.month}", f"{dt.day}", f"{dt:%H:%M:%S.%f}",
                    Path(filename).stem + '.mp4'])
    except Exception as Err:
        logging.error('Converting validation video error: ' + str(Err))
        sms_alert('Video', 'Validation Conversion Error: ' + str(Err))


with picamera.PiCamera() as camera:
    try:
        camera.resolution = (video_width, video_height)
        camera.iso = iso
        camera.framerate = fr
        while True:
            general_time = datetime.now()
            logging = get_logger(general_time)
            hour_int = int(f"{general_time:%H}")
            minute_int = int(f"{general_time:%M}")
            sec_int = int(f"{general_time:%S}")
            for slot in video_time_range:
                if int(slot[0]) <= hour_int < int(slot[1]):
                    dt = general_time
                    print('Validation recording started')
                    dt_str = dt.strftime("%Y_%m_%d_%H_%M_%S")
                    file1_h264 = path + str(box_id) + dt_str + "_validation.h264"
                    camera.start_recording(file1_h264)
                    camera.wait_recording(record_duration)
                    camera.stop_recording()
                    print('Recorded video starting at ' + f"{dt:%H}:{dt:%M}:{dt:%S}")
                    convert_video(file1_h264)
                    print('Recorded and converted video to mp4')
                    if pause > 0:
                        sleep(pause)
    except Exception as E:
        print('Video error: ' + str(E))
        logging.error('Video: ' + str(E))
        sms_alert('Video', 'Error: ' + str(E))
        camera.close()
        logging.info("Camera closed")
    finally:
        camera.stop_recording()
        camera.close()
        logging.info("Camera closed")
