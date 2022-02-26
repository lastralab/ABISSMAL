# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 11/29/21
# !/usr/bin/env python3
import datetime
import logging
import time
import signal
import sys
import os
import csv
from datetime import date
from os.path import exists
import smtplib
from Setup.email_service import source
from Setup.email_service import key

box_id = 'Box_01'
modules = ['IRBB', 'RFID', 'Temp', 'Video']
video_extension = '.mp4'
file_extension = '.csv'
emails = []


def set_logger(default_dir):
    day = date.today()
    today = str(day.year) + "_" + str(day.month) + "_" + str(day.day)
    format_log = "%(asctime)s %(levelname)s %(message)s"
    logging.getLogger(default_dir + 'log/' + today + '_pct_' + box_id + '.log')
    logging.basicConfig(
        format=format_log,
        filename=default_dir + 'log/' + today + '_pct_' + box_id + '.log',
        level=logging.DEBUG,
        datefmt="%Y-%m-%d %H:%M:%S"
    )


def logger_setup(default_dir):
    try:
        if not os.path.exists(default_dir + 'log'):
            os.makedirs(default_dir + 'log')
        main_data = default_dir + 'Data_ParentalCareTracking'
        if not os.path.exists(main_data):
            os.makedirs(main_data)
        irbb_data = main_data + "/IRBB"
        rfid_data = main_data + "/RFID"
        temp_data = main_data + "/Temp"
        video_data = main_data + "/Video"
        if not os.path.exists(irbb_data):
            os.makedirs(irbb_data)
        if not os.path.exists(rfid_data):
            os.makedirs(rfid_data)
        if not os.path.exists(temp_data):
            os.makedirs(temp_data)
        if not os.path.exists(video_data):
            os.makedirs(video_data)
        set_logger(default_dir)
    except Exception as E:
        print('Helper Logger Setup Error: ' + str(E))
        email_alert('Helper', 'Logger Setup Error: ' + str(E))


def csv_writer(box_id, module, data_path, datestring, header, value):
    try:
        if data_path:
            filename = module + '_' + box_id + '_' + datestring + '.csv'
            full_path = data_path + "/" + filename
            if exists(full_path):
                file = open(full_path, 'a+')
                tmp_writer = csv.writer(file)
                tmp_writer.writerow(value)
                file.close()
            else:
                file = open(full_path, 'w+')
                tmp_writer = csv.writer(file)
                tmp_writer.writerow(header)
                tmp_writer.writerow(value)
                file.close()
    except Exception as E:
        logging.error('Helper CSV Writter Error: ' + str(E))
        email_alert('Helper', 'CSV Writter Error: ' + str(E))


def email_alert(module, text):
    try:
        if source != 'email@gmail.com':
            server = smtplib.SMTP('smtp.gmail.com', 587)
            server.starttls()
            server.login(source, key)
            subject = 'PCT[' + box_id + ']: Error from ' + module + ' module'
            msg = 'Subject: {}\n\n{}'.format(subject, text)
            for email in emails:
                server.sendmail(source, email, msg)
                logging.info('Email alert sent to ' + email + ' from ' + module)
                print('Email alert sent to ' + email + ' from ' + module)
            server.quit()
        else:
            logging.error('Email service is not configured, use run_install.sh or emails won\'t be sent.')
            print('Email is not configured, update Setup/email_service.py or emails won\'t be sent.')
    except Exception as Exc:
        logging.error('Helper Email Alert Error: ' + str(Exc))
        print('Email Alert error: ' + str(Exc))
