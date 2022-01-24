# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 11/29/21

import logging
import time
import signal
import sys
import os
import csv
# import RPi.GPIO as GPIO
from datetime import date
from os.path import exists
import smtplib
from setup.email_service import source
from setup.email_service import key

box_id = 'Box_01'
modules = ['IRBB', 'RFID', 'Temp', 'Video']
video_extension = '.mp4'
file_extension = '.csv'
emails = ['gsvidaurre+pct@gmail.com', 'lastralab+pct@gmail.com']


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
    server = smtplib.SMTP('smtp.gmail.com', 587)
    server.starttls()
    server.login(source, key)
    for email in emails:
        subject = 'Module[' + module + ']'
        msg = EmailMessage()
        msg.set_content(text)
        msg['Subject'] = f'PCT Alert: {subject}'
        msg['From'] = source
        msg['To'] = email
        server.sendmail(source, email, msg)
        server.quit()
