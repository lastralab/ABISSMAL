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
from helper import video_extension
from helper import file_extension
from helper import email_alert
import os
from datetime import *
import shutil

pi_home = '/home/pi/'

logger_setup(pi_home)

backup_hour = 20
backup_minute = 15

media_path = '/media/pi/'
data_path = 'Data_ParentalCareTracking/'

logging.info('Started backup monitoring...')
print('Started backup monitoring...')


def usb_connected(box_id):
    if len(os.listdir(media_path)) > 0:
        for volume in os.listdir(media_path):
            if str(volume) == box_id:
                return True
    else:
        exception = 'External drive not detected, backup won\'t be possible.'
        print('Backups error: ' + exception)
        logging.error('Backups error: ' + exception)
        email_alert('Backups', 'Error: ' + exception)


def video_backup_init(dt, date, destination, source):
    src = source + 'Video'
    path = media_path + destination + '/Data/Video/' + date
    files = os.listdir(src)
    if len(files) > 0:
        if not os.path.exists(path):
            os.makedirs(path)
        for filename in files:
            if filename.endswith(video_extension):
                shutil.move(os.path.join(src, filename), os.path.join(path, filename))
                print('Backed-up videos at ' + str(dt.hour) + ':' + str(dt.minute).zfill(2) + 'hrs')
                logging.info('Backed-up videos at ' + str(dt.hour) + ':' + str(dt.minute).zfill(2) + 'hrs')
            if filename.endswith('.h264'):
                os.remove(os.path.join(src, filename))
            else:
                pass
    else:
        pass


def csv_backup_init(dt, destination, source):
    for module in modules:
        src = source + module
        files = os.listdir(src)
        yesterday = dt - timedelta(days=1)
        yesterday_file = module + "_" + box_id + "_" + f"{yesterday.year}_{yesterday.month}_{yesterday.day}" + ".csv"
        if module != 'Video':
            path = media_path + destination + '/Data/' + module + '/'
        else:
            path = media_path + destination + '/Data/Video/' + yesterday.strftime("%Y_%m_%d")
        if len(files) > 0:
            if not os.path.exists(path):
                os.makedirs(path)
            for filename in files:
                if filename.endswith(file_extension):
                    if filename == yesterday_file:
                        shutil.move(os.path.join(src, filename), os.path.join(path, filename))
                        print(
                            'Backed-up ' + module + ' metadata at ' + str(dt.hour) + ':' + str(dt.minute).zfill(
                                2) + 'hrs')
                        logging.info(
                            'Backed-up ' + module + ' metadata at ' + str(dt.hour) + ':' + str(dt.minute).zfill(
                                2) + 'hrs')
                    else:
                        pass
                else:
                    pass
        else:
            pass


try:
    while True:
        now = datetime.now()
        folder = now.strftime("%Y_%m_%d")
        if usb_connected(box_id) and now.hour == backup_hour and now.minute == backup_minute:
            video_backup_init(now, folder, box_id, pi_home + data_path)
            csv_backup_init(now, box_id, pi_home + data_path)
except KeyboardInterrrupt:
    print('Exiting backups')
    logging.info('Exiting backups')
except Exception as E:
    print('Backups error: ' + str(E))
    logging.error('Backups error: ' + str(E))
    email_alert('Backups', 'Error: ' + str(E))
