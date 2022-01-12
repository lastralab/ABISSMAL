# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 1/12/22

# !/usr/bin/env python3

import sys
import logging
from helper import logger_setup
from helper import csv_writer
from helper import box_id
from helper import modules
import os
from datetime import *
import shutil

pi_home = '/home/pi/'

logger_setup(pi_home)

media_path = '/media/pi'
data_path = 'Data_ParentalCareTracking/'

logging.info('Started backups monitoring...')


def usb_connected(box_id):
    if len(os.listdir(media_path)) > 0:
        for volume in os.listdir(media_path):
            if str(volume) == box_id:
                return True
            else:
                logging.error('External drive not detected, backup won\'t be possible.')
                # TODO send email


def backup_init(destination, source):
    dt = datetime.now()
    date = dt.strftime("%m_%d_%Y")
    if usb_connected(destination) and dt.hour > 20:
        for module in modules:
            src = source + '/' + module
            path = media_path + destination + '/Data/' + module + '/' + date
            files = os.listdir(src)
            if len(files) > 0:
                if not os.path.exists(path):
                    os.makedirs(path)
                for filename in files:
                    shutil.move(os.path.join(source + '/' + module, filename), os.path.join(path, filename))
                logging.info('Backed-up ' + module + 'data at ' + str(dt.hour) + ':' + str(dt.minute) + 'hrs')


try:
    while True:
        backup_init(box_id, pi_home + data_path)
except KeyboardInterrupt:
    logging.info('Exiting Backups.py')
