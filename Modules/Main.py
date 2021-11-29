# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 11/29/21

import threading
import logging
import time
import signal
import sys
import error_alert
import RPi.GPIO as GPIO
from datetime import date
from IRBB import detect_beam_breaks_callback
from IRBB import signal_handler
from os.path import exists
from EmailService import error_alert

# Constants
box_id = 101
modules = ['init_irbb', 'init_rfid', 'init_temp', 'init_video', 'init_backup']
logfile = 'path'  # todo implement
format = "%(asctime)s: %(message)s"
logging.basicConfig(format=format, level=logging.INFO,
                    datefmt="%H:%M:%S")


# CSV main handler function
# todo add timeout to track pin malfunction
def csv_writer(box, module, date, value):
    filename = box + '_' + date + '.csv'
    full_path = module + '/Data/' + filename
    if exists(full_path):
        file = open(full_path, 'a+')
        file.write(value)
    else:
        file = open(full_path, 'w+')
        file.write(value)


# IRBB - IR Beam Breaker function
def init_irbb(box):
    BEAM_PIN = 16
    module = 'IRBB'
    GPIO.setmode(GPIO.BCM)
    GPIO.setup(BEAM_PIN, GPIO.IN, pull_up_down=GPIO.PUD_UP)

    timestamp = GPIO.add_event_detect(BEAM_PIN, GPIO.FALLING, callback=detect_beam_breaks_callback,
                                      bouncetime=100)
    if not timestamp:
        logging.info('No IRBB activity on pin ' + BEAM_PIN + '.')
    else:
        logging.info('IRBB activity detected.')
        if isinstance(timestamp, datetime.datetime):
            csv_writer(box, module, date.today(), timestamp)
        else:
            msg = 'IRBB is not returning a timestamp: ' + timestamp
            logging.error(msg)
            # error_alert.email_alert('gsvidaurre@gmail.com', box + '_' + module, msg)


if __name__ == "__main__":
    logging.info("Main: before creating thread")
    irbb = threading.Thread(target=init_irbb, args=(box_id,))
    logging.info("Main: before running thread")
    irbb.start()
    logging.info("Main: all done")
    # irbb.run() ?

    if not irbb.is_alive():
        logging.error('IRBB is not running')
