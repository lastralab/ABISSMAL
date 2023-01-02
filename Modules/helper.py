# Created by PyCharm
# Author: nmoltta, gsvidaurre
# Project: Abissmal
# Date: 11/29/21

# !/usr/bin/env python3

import datetime
import time
import sys
import os
import csv
from datetime import date
from os.path import exists
import smtplib
from Setup.twilioSMS import Sender
from Setup.twilioSMS import Recipients
from twilio.rest import Client
from Setup.twilioSMS import Enabled
from Setup.twilioSMS import Sid
from Setup.twilioSMS import Token
import log

box_id = 'Box_01'
modules = []
video_extension = '.mp4'
file_extension = '.csv'


def get_logger(day):
    name = str(day.year) + '_' + str(day.month) + '_' + str(day.day) + '_abissmal_' + box_id + '.log'
    logger = log.setup_custom_logger(name)
    return logger


def dir_setup(default_dir):
    try:
        if not os.path.exists(default_dir + 'log'):
            os.makedirs(default_dir + 'log')
        main_data = default_dir + 'Data_Abissmal'
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
    except Exception as E:
        print('Helper Logger Setup Error: ' + str(E))
        sms_alert('Helper', 'Logger Setup Error: ' + str(E))


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
        logging = get_logger(datetime.date.today())
        logging.error('Helper CSV Writter Error: ' + str(E))
        sms_alert('Helper', 'CSV Writter Error: ' + str(E))


def sms_alert(module, text):
    today = date.today()
    logging = get_logger(today)
    try:
        if Enabled and Sid != '' and Token != '' and Sender != '' and Recipients != []:
            msg = 'Abissmal[' + box_id + '-' + module + '] ' + text
            for recipient in Recipients:
                client = Client(Sid, Token)
                message = client.messages.create(
                    to='+1'+recipient,
                    from_='+1'+Sender,
                    body=msg)
                logging.info('SMS sent to ...' + recipient[-4:] + ' from ' + module)
                logging.info(message.sid)
                print('SMS sent to ...' + recipient[-4:] + ' from ' + module)
        else:
            logging.info('Twilio service is not configured, use run_install.sh or SMS won\'t be sent.')
            print('Twilio is not configured, update Setup/email_service.py or emails won\'t be sent.')
    except Exception as Exc:
        logging.error('Helper sending SMS Error: ' + str(Exc))
        print('Sending SMS error: ' + str(Exc))
