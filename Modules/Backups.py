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

backup_hour = 10
backup_minute = 10

media_path = '/media/pi/'
data_path = pi_home + 'Data_ParentalCareTracking/'
log_path = '/home/pi/log/'

logging.info('Started backup script.')
print('Started backup script.')


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
        if not os.path.exists(path):
            os.makedirs(path)
        for filename in files:
            if filename.endswith(video_extension):
                shutil.move(os.path.join(src, filename), os.path.join(path, filename))
                print('Backed-up video ' + filename)
                logging.info('Backed-up video ' + filename)
            else:
                pass
            if filename.endswith('.h264'):
                os.remove(os.path.join(src, filename))
                print('Deleted video from source: ' + filename)
                logging.info('Deleted video from source' + filename)
            else:
                pass
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


def logs_backup_init(day, destination, source):
    today = str(day.year) + "_" + str(day.month) + "_" + str(day.day)
    yday = day - timedelta(days=1)
    ydate = str(yday.year) + "_" + str(yday.month) + "_" + str(yday.day)
    logs = os.listdir(source)
    if len(logs) > 0:
        path = destination + '/Data/Logs/'
        logfile = log_path + 'pct_' + box_id + '.log'
        to_backup = log_path + today + '_pct_' + box_id + '.log'
        if os.path.exists(logfile):
            os.rename(logfile, to_backup)
            with open(logfile, mode='a'):
                pass
            logger_setup(pi_home)
            logging.info('Created log file to backup tomorrow: ' + to_backup)
            print('Created log file to backup tomorrow.')
        else:
            with open(logfile, mode='a'):
                pass
            logging.info('Created log file to backup today: ' + logfile)
            print('Created log file to backup today.')
        if not os.path.exists(path):
            os.makedirs(path)
        for log in logs:
            if log.endswith('.log'):
                yesterday_log = ydate + '_pct_' + box_id + '.log'
                if log == yesterday_log:
                    shutil.move(os.path.join(log_path, log), os.path.join(path, log))
                    print('Backed-up log')
                    logging.info('Backed-up log')
                else:
                    print('Backup Warning: Log file from yesterday not found.')
                    logging.warning('Backup Error: Log file from yesterday not found.')
                    email_alert('Backup', 'Warning: Log file from yesterday not found.')
                    pass
            else:
                print('Backup Warning: .log files not found.')
                logging.warning('Backup: .log files not found.')
                email_alert('Backup', 'Warning: .log files not found.')
                pass
    else:
        print('Backup Error: Logs not found in /home/pi/log/')
        logging.error('Backup Error: Logs not found in /home/pi/log/')
        email_alert('Backup', 'Error: Logs not found in /home/pi/log/')
        pass


try:
    while True:
        now = datetime.now()
        if usb_connected(box_id) and now.hour == backup_hour and now.minute == backup_minute:
            folder = now.strftime("%Y_%m_%d")
            video_backup_init(folder, media_path + box_id, data_path)
            csv_backup_init(now, media_path + box_id, data_path)
            logs_backup_init(now, media_path + box_id, log_path)
            time.sleep(61)
        pass
except KeyboardInterrrupt:
    print('Exiting backups')
    logging.info('Exiting backups')
except Exception as E:
    print('Backups error: ' + str(E))
    logging.error('Backups error: ' + str(E))
    email_alert('Backups', 'Error: ' + str(E))
