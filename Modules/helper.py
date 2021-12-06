# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 11/29/21

import logging
import time
import signal
import sys
import RPi.GPIO as GPIO
from datetime import date
from os.path import exists
import smtplib
from email.message import EmailMessage

# Constants and logging setup
box_id = 101
modules = ['IRBB', 'RFID', 'Temp']
format = "%(asctime)s: %(message)s"


logging.basicConfig(
    format=format,
    filename='/home/pi/log/info.log',
    level=logging.INFO,
    datefmt="%H:%M:%S"
)

logging.basicConfig(
    format=format,
    filename='/home/pi/log/error.log',
    level=logging.ERROR,
    datefmt="%H:%M:%S"
)

logging.basicConfig(
    format=format,
    filename='/home/pi/log/warning.log',
    level=logging.WARNING,
    datefmt="%H:%M:%S"
)


# CSV helper function
# TODO: add timeout to track pin malfunction
def csv_writer(box, module, date, value):
    if module in modules:
        filename = box + '_' + date + '.csv'
        full_path = module + '/Data/' + filename
        if exists(full_path):
            file = open(full_path, 'a+')
            file.write(value)
        else:
            file = open(full_path, 'w+')
            file.write(value)


# Email Service helper function
# TODO: test
def email_alert(toemail, module, text):
    subject = 'Module[' + module + ']'
    msg = EmailMessage()
    msg.set_content(text)
    msg['Subject'] = f'Pi Alert: {subject}'
    msg['From'] = ''  # smtp localhost setup email
    msg['To'] = toemail

    s = smtplib.SMTP('localhost')
    s.send_message(msg)
    s.quit()
