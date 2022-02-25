# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 1/12/22

# !/usr/bin/env python3

import sys
import logging
from helper import logger_setup
from helper import box_id
from helper import modules
from helper import video_extension
from helper import file_extension
from helper import email_alert
import os
from datetime import *
import shutil
import time

pi_home = '/home/pi/'
logger_setup(pi_home)

backup_hour = 20
backup_minute = 15
media_path = '/media/pi/'
data_path = pi_home + 'Data_ParentalCareTracking/'

logging.info('Started backup script')
print('Started backup script')

logging.info('Backups will run once at ' + str(backup_hour) + ':' + str(backup_minute) + 'hrs every day')
print('Backups will start at ' + str(backup_hour) + ':' + str(backup_minute) + 'hrs')


def usb_connected(box):
    if len(os.listdir(media_path)) > 0:
        for volume in os.listdir(media_path):
            if str(volume) == box:
                return True
    else:
        exception = 'External drive not detected, backup won\'t be possible.'
        print('Backups error: ' + exception)
        logging.error('Backups error: ' + exception)
        email_alert('Backups', 'Error: ' + exception)


def video_backup_init(foldername, destination, source):
    src = source + 'Video'
    path = destination + '/Data/Video/' + foldername
    files = os.listdir(src)
    if len(files) > 0:
        videos = 0
        deleted = 0
        if not os.path.exists(path):
            os.makedirs(path)
        for filename in files:
            if filename.endswith(video_extension):
                shutil.move(os.path.join(src, filename), os.path.join(path, filename))
                print('Backed-up video ' + filename)
                videos = videos + 1
            else:
                pass
            if filename.endswith('.h264'):
                os.remove(os.path.join(src, filename))
                print('Deleted video from source: ' + filename)
                deleted = deleted + 1
            else:
                pass
        print('Backed-up ' + str(videos) + ' videos')
        print('Deleted ' + str(deleted) + ' .h264 videos')
        logging.info('Backed-up ' + str(videos) + ' videos')
        logging.info('Deleted ' + str(deleted) + ' .h264 videos')
    else:
        print('No videos backed-up. No files found')
        logging.error('No videos backed-up. No files found. Check camera/video module.')
        email_alert('Backup', 'No videos backed-up. Check camera/video module. No files found in source: ' + source)
        pass


def csv_backup_init(today, destination, source):
    today_string = str(today.year) + "_" + str(today.month) + "_" + str(today.day)
    for module in modules:
        src = source + module
        files = os.listdir(src)
        today_file = module + '_' + box_id + "_" + today_string + ".csv"
        path = destination + '/Data/' + module + '/'
        if len(files) > 0:
            if not os.path.exists(path):
                os.makedirs(path)
            for filename in files:
                if filename.endswith(file_extension):
                    if filename != today_file:
                        shutil.move(os.path.join(src, filename), os.path.join(path, filename))
                    else:
                        pass
                else:
                    pass
            print('Backed-up ' + module + ' metadata')
            logging.info('Backed-up ' + module + ' metadata')
        else:
            print('CSV files not found in module: ' + module)
            logging.warning('Backup: CSV files not found in module: ' + module)
            email_alert('Backup', 'Warning: CSV files not found in module: ' + module)
            pass


try:
    while True:
        now = datetime.now()
        if usb_connected(box_id) and now.hour == backup_hour and now.minute == backup_minute:
            folder = now.strftime("%Y_%m_%d")
            video_backup_init(folder, media_path + box_id, data_path)
            csv_backup_init(now, media_path + box_id, data_path)
            time.sleep(61)
        pass
except KeyboardInterrrupt:
    print('Exiting backups')
    logging.info('Exiting backups')
except Exception as E:
    print('Backups error: ' + str(E))
    logging.error('Backups error: ' + str(E))
    email_alert('Backups', 'Error: ' + str(E))
