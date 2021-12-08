# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 11/29/21

import logging
import time
import signal
import sys
import os
# import RPi.GPIO as GPIO
from datetime import date
from os.path import exists
import smtplib
# from email.message import EmailMessage


def logger_setup(default_dir):
    # Create the log folder unless it already exists
    if not os.path.exists(default_dir + 'log'):
        os.makedirs(default_dir + 'log')

    # Create the main folder where data will be saved unless this already exists
    main_data = default_dir + 'Data_ParentalCareTracking'
    if not os.path.exists(main_data):
        os.makedirs(main_data)

    # Set path variables for module data, then create these sub-directories if they don't already exist
    irbb_data = main_data + "/IRBB"
    rfid_data = main_data + "/RFID"
    temp_data = main_data + "/Temp"
    video_data = main_data + "/RFID/Data/Video"

    if not os.path.exists(irbb_data):
        os.makedirs(irbb_data)

    if not os.path.exists(rfid_data):
        os.makedirs(rfid_data)

    if not os.path.exists(temp_data):
        os.makedirs(temp_data)

    if not os.path.exists(video_data):
        os.makedirs(video_data)

    # Constants and logging setup
    box_id = 101
    modules = ['IRBB', 'RFID', 'Temp']
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


# CSV helper function
# TODO: add timeout to track pin malfunction
def csv_writer(box, data_path, date, value):
    if data_path:
        filename = box + '_' + date + '.csv'
        full_path = data_path + filename
        if exists(full_path):
            file = open(full_path, 'a+')
            file.write(value)
        else:
            file = open(full_path, 'w+')
            file.write(value)


# Email Service helper function
# TODO: test
# def email_alert(toemail, module, text):
#     subject = 'Module[' + module + ']'
#     msg = EmailMessage()
#     msg.set_content(text)
#     msg['Subject'] = f'Pi Alert: {subject}'
#     msg['From'] = ''  # smtp localhost setup email
#     msg['To'] = toemail
#
#     s = smtplib.SMTP('localhost')
#     s.send_message(msg)
#     s.quit()


# TODO add backup function
