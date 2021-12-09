# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 12/8/21

import signal
import sys
import logging
import os
from datetime import date
from os.path import exists
from helper import logger_setup
from picamera import PiCamera
from time import sleep
from subprocess import call


logger_setup('/home/pi/')


def convert(file_h264):

    command = "MP4Box -add " + file_h264 + " " + file_mp4
    call([command], shell=True)
    logging.info('Converted video h264 to mp4.')


# TODO test adding function to beam breaker function, passing time param
# TODO try catch, log errors
def record(time_now):
    path = home_pi + 'RFID/Data/Video/'
    filename = 'IRBB_' + time_now
    file_h264 = path + filename + '.h264'
    camera = PiCamera()
    camera.resolution = (640, 480)
    camera.framerate = 15
    camera.start_preview()
    camera.start_recording(file_h264)
    sleep(60)
    camera.stop_recording()
    camera.stop_preview()
    file_mp4 = path + filename + '.mp4'
    command = "MP4Box -add " + file_h264 + " " + file_mp4
    call([command], shell=True)
    logging.info('Converted video ' + filename + ' to mp4.')

# test
# print('IRBB ran...')
# logging.info('IRBB running')
