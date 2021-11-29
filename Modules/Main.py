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

# Box identification
box_id = 101


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
def init_irbb(box, run):
    while run:
        BEAM_PIN = 16
        module = 'IRBB'
        GPIO.setmode(GPIO.BCM)
        GPIO.setup(BEAM_PIN, GPIO.IN, pull_up_down=GPIO.PUD_UP)

        timestamp = GPIO.add_event_detect(BEAM_PIN, GPIO.FALLING, callback=detect_beam_breaks_callback,
                                          bouncetime=100)
        if not timestamp:
            logging.info('No IRBB activity on pin ' + BEAM_PIN + '.')
        else:
            logging.info('IRBB activity detected: ' + timestamp)
            if isinstance(timestamp, datetime.datetime):
                csv_writer(box, module, date.today(), timestamp)
            else:
                msg = 'IRBB is not returning a timestamp: ' + timestamp
                logging.error(msg)
                # error_alert.email_alert('gsvidaurre@gmail.com', box + '_' + module, msg)

        # return true ?


# IRBB example that should be possible with threading without needing the "run" parameter
# init_irbb(box_id, run = true)

init_irbb(box_id, true)

# todo try init_irbb(box_id, false)

threading.Thread()
