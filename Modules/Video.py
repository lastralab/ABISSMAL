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
from moviepy.editor import VideoFileClip
from moviepy.editor import concatenate_videoclips

dir_setup('/home/pi/')
logging = get_logger(datetime.today())
logging.info('Starting Video script')
print('Started Video script')

path = "/home/pi/Data_Abissmal/Video/"
header = ['chamber_id', 'sensor_id', 'year', 'month', 'day', 'time_video_started', 'video_file_name', 'total_pixels_motionTrigger']
prior_image = None
video_time_range = [0, 23]
video_width = 1280
video_height = 720
iso = 400
fr = 30
stream_duration = 6
record_duration = 9
threshold = 50
sensitivity = 9000
REC_LED = 16
LED_time_range = [6, 18]
GPIO.setmode(GPIO.BCM)
GPIO.setwarnings(False)
GPIO.setup(REC_LED, GPIO.OUT)
GPIO.PWM(REC_LED, 40)

logging.info('Video will be recorded between ' + str(video_time_range[0]) + ' and ' + str(video_time_range[1] + 1) + ' hours')
print('Video will be recorded between ' + str(video_time_range[0]) + ' and ' + str(video_time_range[1] + 1) + ' hours')


def set_prior_image(current_image):
    global prior_image
    prior_image = current_image


def detect_motion(cam):
    global prior_image
    streaming = io.BytesIO()
    cam.capture(streaming, format='jpeg', use_video_port=True)
    streaming.seek(0)
    if prior_image is None:
        prior_image = Image.open(streaming)
        return [False]
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
            date = datetime.now()
            logging.debug('Video: sensitivity = ' + str(sensitivity) + ' < ' + str(pixels) + ' pixels')
            result = [True, pixels, date]
        else:
            result = [False, None, None]
        set_prior_image(current_image)
        return result


def convert_video(filename, pixels, dt):
    try:
        file_mp4 = path + Path(filename).stem + '.mp4'
        command = "MP4Box -add " + filename + " " + file_mp4
        call([command], shell=True)
        print('Converted video')
        os.remove(filename)
        csv_writer(str(box_id), 'Video', path, f"{dt.year}_{dt.month}_{dt.day}",
                   header,
                   [box_id, 'Camera', f"{dt.year}", f"{dt.month}", f"{dt.day}", f"{dt:%H:%M:%S.%f}", Path(filename).stem + '.mp4', pixels])
        return file_mp4
    except Exception as Err:
        logging.error('Converting video error: ' + str(Err))
        sms_alert('Video', 'Convert Error: ' + str(Err))
        return False


def concatenate(v1, v2, name):
    try:
        from moviepy.editor import VideoFileClip
        clip1 = VideoFileClip(v1)
        clip2 = VideoFileClip(v2)
        final_clip = concatenate_videoclips([clip1, clip2])
        final_clip.write_videofile(name + '.mp4', codec='libx264', audio_codec='aac', ffmpeg_params=['-hide_banner'])
        os.remove(v1)
        os.remove(v2)
    except Exception as Damn:
        logging.error('Concatenation error: ' + str(Damn))
        sms_alert('Video', 'Concatenation Error: ' + str(Damn))


with picamera.PiCamera() as camera:
    try:
        camera.resolution = (video_width, video_height)
        camera.iso = iso
        camera.framerate = fr
        stream = picamera.PiCameraCircularIO(camera, seconds=stream_duration)
        camera.start_recording(stream, format='h264')
        while True:
            general_time = datetime.now()
            logging = get_logger(general_time)
            hour_int = int(f"{general_time:%H}")
            motion = detect_motion(camera)
            if (int(video_time_range[0]) <= hour_int <= int(video_time_range[1])) and motion[0]:
                dt = motion[2]
                print('Motion detected; Recording started')
                logging.info("Motion detected. Starting video recordings")
                dt_str = str(f"{dt.year}-{dt.month}-{dt.day}_{dt:%H}-{dt:%M}-{dt:%S}")
                file1_h264 = path + str(box_id) + "_" + dt_str + "_pre" + '.h264'
                file2_h264 = path + str(box_id) + "_" + dt_str + "_post" + '.h264'
                if int(LED_time_range[0]) <= hour_int <= int(LED_time_range[1]):
                    GPIO.output(REC_LED, GPIO.HIGH)
                camera.split_recording(file2_h264)
                camera.wait_recording(record_duration)
                camera.split_recording(stream)
                stream.copy_to(file1_h264, seconds=stream_duration)
                stream.clear()
                streaming = io.BytesIO()
                camera.capture(streaming, format='jpeg', use_video_port=True)
                streaming.seek(0)
                set_prior_image(Image.open(streaming))
                print('Recording finished')
                logging.info("Videos recorded")
                if int(LED_time_range[0]) <= hour_int <= int(LED_time_range[1]):
                    GPIO.output(REC_LED, GPIO.LOW)
                pre = convert_video(file1_h264, motion[1], dt)
                post = convert_video(file2_h264, motion[1], dt)
                print('Converted videos to mp4')
                logging.info("Converted videos to mp4")
                if pre and post:
                    concatenate(pre, post, path + dt_str)
                    print('Concatenated videos to one file')
                    logging.info("Concatenated videos to one file")
    except Exception as E:
        print('Video error: ' + str(E))
        logging.error('Video: ' + str(E))
        sms_alert('Video', 'Error: ' + str(E))
    finally:
        camera.stop_recording()
        camera.close()
        GPIO.output(REC_LED, GPIO.LOW)
