# Created by PyCharm
# Author: nmoltta, gsvidaurre
# Project: ParentalCareTracking
# Date: 11/16/2021

import time
import signal
from datetime import datetime
import sys
import csv
import RPi.GPIO as GPIO
import logging
from helper import logger_setup
from helper import csv_writer
from helper import box_id
from helper import irbb_data

BEAM_PIN = 16
warn = 0
module = 'IRBB'

logger_setup()

# CSV header
header = ['chamber_id', 'sensor_id', 'year', 'month', 'day', 'timestamp']
sensor_id = "lead"

def detect_beam_breaks_callback(BEAM_PIN):
    # Return date and timestamp when the beam is broken
    if not GPIO.input(BEAM_PIN):
        return()
        #logging.info('No IRBB activity on pin ' + str(BEAM_PIN) + '.')
    else:
        dt = datetime.now()
        
        # Logging is not working at all, nothing is written
        #logging.info('IRBB activity detected')
        #logging.INFO('IRBB activity detected at: ' + f"{dt:%H:%M:%S.%f}")
        # This finally prints a .csv but the callback isn't working
        csv_writer(str(box_id), 'IRBB', irbb_data, f"{dt.year}_{dt.month}_{dt.day}",
                   header, [box_id, sensor_id, f"{dt.year}", f"{dt.month}", f"{dt.day}", f"{dt:%H:%M:%S.%f}"])
        # TODO implement video function


# Handler function for manual Ctrl + C cancellation
def signal_handler(sig, frame):
    GPIO.cleanup()
    sys.exit(0)
    
GPIO.setmode(GPIO.BCM)
GPIO.setup(BEAM_PIN, GPIO.IN, pull_up_down=GPIO.PUD_UP)
    
# Add event detection for beam breaker with the callback function
# Not working, even when I add a print to the callback function
GPIO.add_event_detect(BEAM_PIN, GPIO.FALLING, callback=detect_beam_breaks_callback,
                                      bouncetime=100)

# TODO test: helper.email_alert('gsvidaurre@gmail.com', box + '_' + module, msg)
