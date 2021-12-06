# Created by PyCharm
# Author: nmoltta, gsvidaurre
# Project: ParentalCareTracking
# Date: 11/16/2021

import time
import signal
from datetime import date
import sys
import RPi.GPIO as GPIO
from Modules import helper
from helper import box_id

BEAM_PIN = 16
warn = 0
module = 'IRBB'
GPIO.setmode(GPIO.BCM)
GPIO.setup(BEAM_PIN, GPIO.IN, pull_up_down=GPIO.PUD_UP)


def detect_beam_breaks_callback(BEAM_PIN):
    # Return date and timestamp when the beam is broken
    if not GPIO.input(BEAM_PIN):
        helper.logging.info('No IRBB activity on pin ' + BEAM_PIN + '.')
    else:
        dt = date.now()
        logging.info('IRBB activity detected at: ' + dt)
        helper.csv_writer(box_id, module, dt, timestamp)
        # TODO implement video function


# Handler function for manual Ctrl + C cancellation
def signal_handler(sig, frame):
    GPIO.cleanup()
    sys.exit(0)

# TODO test: helper.email_alert('gsvidaurre@gmail.com', box + '_' + module, msg)
