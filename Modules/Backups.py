# Created by PyCharm
# Author: tanismo
# Project: Abissmal
# Date: 1/12/22

# !/usr/bin/env python3
import datetime
import sys
from helper import dir_setup
from helper import box_id
from helper import modules
from helper import video_extension
from helper import file_extension
from helper import sms_alert
import os
from datetime import *
import shutil
import time
from helper import get_logger
import subprocess
import re

pi_home = '/home/pi/'
dir_setup(pi_home)

backup_hour = 20
backup_minute = 15
low_storage = 200
media_path = '/media/pi/'
data_path = pi_home + 'Data_Abissmal/'
log_path = '/home/pi/log/'
volume_id = ''
logging = get_logger(datetime.today())

logging.info('Started backup script')
print('Started backup script')

logging.info('Data transfer will run once every day at ' + str(backup_hour) + ':' + str(backup_minute) + 'hrs')
print('Data transfer will run once every day at ' + str(backup_hour) + ':' + str(backup_minute) + 'hrs')
logging.info('Logs transfer will run once at 0:00hrs every day')
print('Logs transfer will run once at 0:00hrs every day')


def usb_connected():
    if len(os.listdir(media_path)) > 0:
        global volume_id
        for volume in os.listdir(media_path):
            if len(os.listdir(media_path + volume)) > 0:
                volume_id = str(volume)
                return True
            pass
        pass
    else:
        exception = 'External drive not detected, backup won\'t be possible.'
        print('Backups error: ' + exception)
        logging.error('Backups error: ' + exception)
        sms_alert('Backups', 'Error: ' + exception)


def video_backup_init(yesterday, destination, source):
    src = source + 'Video'
    today = datetime.now().strftime("%Y_%m_%d")
    path = destination + '/Data/Video/' + yesterday
    files = os.listdir(src)
    if len(files) > 0:
        videos = 0
        if not os.path.exists(path):
            os.makedirs(path)
        for filename in files:
            if filename.endswith(video_extension):
                if yesterday in filename:
                    shutil.move(os.path.join(src, filename), os.path.join(path, filename))
                    print('Backed-up video ' + filename)
                    videos = videos + 1
                else:
                    index = filename.find(box_id + '_') + len(box_id + '_')
                    older = filename[index:index + 10]
                    if older == today:
                        pass
                    else:
                        path = destination + '/Data/Video/' + older
                        if not os.path.exists(path):
                            os.makedirs(path)
                        shutil.move(os.path.join(src, filename), os.path.join(path, filename))
                        print('Backed-up older video ' + filename)
                        videos = videos + 1
            else:
                pass
        print('Backed-up ' + str(videos) + ' videos')
        logging.info('Backed-up ' + str(videos) + ' videos')
    else:
        print('No videos backed-up. No files found')
        logging.error('No videos backed-up. No files found. Check camera/video module.')
        sms_alert('Backup', 'No videos backed-up. Check camera/video module. No files found in source: ' + source)
        pass


def csv_backup_init(today, destination, source):
    today_string = today.strftime("%Y_%m_%d")
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
            print('CSV files not found in module: ' + module + '. Disable this module to stop getting these warnings.')
            logging.warning('Backup: CSV files not found in module: ' + module)
            sms_alert('Backup', 'Warning: CSV files not found in module: ' + module)
            pass


def logs_backup_init(day, destination, source):
    yday = day - timedelta(days=1)
    today = yday.strftime("%Y_%m_%d")
    logs = os.listdir(source)
    if len(logs) > 0:
        logging = get_logger(datetime.today())
        path = destination + '/Data/Logs/'
        today_log = today + '_abissmal_' + box_id + '.log'
        logs_qty = 0
        if not os.path.exists(path):
            os.makedirs(path)
        for log in logs:
            if log.endswith('.log'):
                if not (log == today_log or log == 'abissmal_cron.log'):
                    shutil.move(os.path.join(log_path, log), os.path.join(path, log))
                    logs_qty = logs_qty + 1
                pass
            else:
                print('Backup Warning: .log files not found in source.')
                logging.warning('Backup: .log files not found in source.')
                sms_alert('Backup', 'Warning: .log files not found in source.')
                pass
        print('Backed-up logs: ' + str(logs_qty))
        logging.info('Backed-up logs total: ' + str(logs_qty))
    else:
        logging = get_logger(datetime.today())
        print('Backup Error: Empty Log dir: /home/pi/log/')
        logging.error('Backup: Empty Log dir /home/pi/log/')
        sms_alert('Backup', 'Error: Empty Log directory /home/pi/log/')
        pass


def monitor_storage(path):
    logging = get_logger(datetime.today())
    output = subprocess.check_output('df -h --output=avail ' + path + ' | grep -v Avail', shell=True, text=True)
    gigs = int(''.join(re.findall('[0-9]', output)))
    if gigs <= low_storage:
        msg = 'Warning: External drive available space is lower than ' + str(low_storage) + 'G. Needs to be empty soon.'
        sms_alert('Backup', msg)
        logging.error(msg)
    else:
        logging.info('Storage Monitor: Total space available in external drive: ' + str(gigs) + 'G')


try:
    while True:
        if usb_connected():
            now = datetime.now()
            if now.hour == backup_hour and now.minute == backup_minute:
                yday = now - timedelta(days=1)
                folder = yday.strftime("%Y_%m_%d")
                video_backup_init(folder, media_path + volume_id, data_path)
                csv_backup_init(now, media_path + volume_id, data_path)
                time.sleep(61)
            elif now.hour == 0 and now.minute == 0:
                logs_backup_init(now, media_path + volume_id, log_path)
                monitor_storage(media_path + volume_id)
                logging = get_logger(datetime.today())
                time.sleep(61)
            else:
                pass
        else:
            pass
except Exception as E:
    print('Backups error: ' + str(E))
    logging.error('Backups error: ' + str(E))
    sms_alert('Backups', 'Error: ' + str(E))
