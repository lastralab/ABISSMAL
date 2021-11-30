# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 11/29/21

import os
import threading
import logging
import time
import signal
import sys
import RPi.GPIO as GPIO
from datetime import date

# Add module folders to system path to import functions inside nested folders
sys.path.append("/home/pi/Desktop/ParentalCareTracking/Modules/IRBB")
from IRBB import detect_beam_breaks_callback
from IRBB import signal_handler
from os.path import exists
from EmailService import error_alert

# Create the log folder unless it already exists
if not os.path.exists('/home/pi/log'):
    os.makedirs('/home/pi/log')

# Create the main folder where data will be saved unless this already exists
main_data = '/home/pi/Data_ParentalCareTracking'
if not os.path.exists(main_data):
    os.makedirs(main_data)
    
# Set path variables for module data, then create these sub-directories if they don't already exist
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
    
# Constants and logging setup
box_id = 101
modules = ['init_irbb', 'init_rfid', 'init_temp', 'init_video', 'init_backup']
FORMAT = "%(asctime)s: %(message)s"

logging.basicConfig(
    format=FORMAT,
    filename='/home/pi/log/info.log',
    level=logging.INFO,
    datefmt="%Y-%m-%d %H:%M:%S"
)

logging.basicConfig(
    format=format,
    filename='/home/pi/log/error.log',
    level=logging.ERROR,
    datefmt="%Y-%m-%d %H:%M:%S"
)

logging.basicConfig(
    format=format,
    filename='/home/pi/log/warning.log',
    level=logging.WARNING,
    datefmt="%Y-%m-%d %H:%M:%S"
)


# CSV main handler function
# todo add timeout to track pin malfunction
def csv_writer(box, irbb_data, date, value):
    filename = box + '_' + date + '.csv'
    full_path = irbb_data + filename
    if exists(full_path):
        file = open(full_path, 'a+')
        file.write(value)
    else:
        file = open(full_path, 'w+')
        file.write(value)


# IRBB - IR Beam Breaker function
def init_irbb(box):
    BEAM_PIN = 16
    warn = 0
    module = 'IRBB'
    GPIO.setmode(GPIO.BCM)
    GPIO.setup(BEAM_PIN, GPIO.IN, pull_up_down=GPIO.PUD_UP)

    timestamp = GPIO.add_event_detect(BEAM_PIN, GPIO.FALLING, callback=detect_beam_breaks_callback,
                                      bouncetime=100)
    
    # If timestamp variable is empty then log no activity
    if not timestamp:
        logging.info('No IRBB activity on pin ' + str(BEAM_PIN) + '.')
    else:
        logging.info('IRBB activity detected.')
        if isinstance(timestamp, datetime.datetime):
            csv_writer(box, irbb_data, date.today(), timestamp)
        else:
            warn = warn + 1
            msg = 'IRBB is not returning a timestamp: ' + timestamp
            logging.error(msg) if warn > 3 else logging.warn(msg)
            # error_alert.email_alert('gsvidaurre@gmail.com', box + '_' + module, msg)


if __name__ == "__main__":
    warnings = 0
    logging.info("Main: before creating thread")
    irbb = threading.Thread(target=init_irbb, args=(box_id,))
    logging.info("Main: before running thread")
    irbb.start()
    logging.info("Main: all done")
    # irbb.run() ?

    if not irbb.is_alive():
        warnings = warnings + 1
        msg = 'IRBB thread is not running'
        logging.error(msg) if warnings > 3 else logging.warning(msg)
