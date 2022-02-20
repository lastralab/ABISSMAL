# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 11/29/21
# !/usr/bin/env python3

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


def logger_setup(default_dir):

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

    FORMAT = "%(asctime)s: %(message)s"

    logging.basicConfig(
        format=FORMAT,
        filename=default_dir + 'log/info.log',
        level=logging.INFO,
        datefmt="%Y-%m-%d %H:%M:%S"
    )

    logging.basicConfig(
        format=format,
        filename=default_dir + 'log/error.log',
        level=logging.ERROR,
        datefmt="%Y-%m-%d %H:%M:%S"
    )

    logging.basicConfig(
        format=format,
        filename=default_dir + 'log/warning.log',
        level=logging.WARNING,
        datefmt="%Y-%m-%d %H:%M:%S"
    )


def csv_writer(box_id, module, data_path, date, header, value):
    if data_path:
        filename = module + '_' + box_id + '_' + date + '.csv'
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


def email_alert(module, text):
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
        logging.error('Helper error: Email is not configured, update Setup/email_service.py or emails won\'t be sent.')
        print('Helper error: Email is not configured, update Setup/email_service.py or emails won\'t be sent.')
