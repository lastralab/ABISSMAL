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

# GPIO pin ID through which IR receiver transmits data
BEAM_PIN = 16
warn = 0
module = 'IRBB'


def break_beam_callback():
    if GPIO.input(BEAM_PIN):
        print("beam unbroken")
    else:
        print("beam broken")


# test:
logger_setup()

GPIO.setmode(GPIO.BCM)
GPIO.setup(BEAM_PIN, GPIO.IN, pull_up_down=GPIO.PUD_UP)
GPIO.add_event_detect(BEAM_PIN, GPIO.BOTH, callback=break_beam_callback)

message = input("Press enter to quit\n\n")
GPIO.cleanup()


# CSV header
# header = ['chamber_id', 'sensor_id', 'year', 'month', 'day', 'timestamp']
# sensor_id = "lead"
#
# GPIO.setmode(GPIO.BCM)
# GPIO.setup(BEAM_PIN, GPIO.IN, pull_up_down=GPIO.PUD_UP)
#
# def detect_beam_breaks_callback(BEAM_PIN):
#     # Return date and timestamp when the beam is broken (previous)
#     # GPIO.add_event_detect(BEAM_PIN, GPIO.FALLING, callback=detect_beam_breaks_callback,
#     #                                   bouncetime=100)
#
#     if not GPIO.input(BEAM_PIN):
#         loggin.info('nothing')
#         # logging.info('No IRBB activity on pin ' + str(BEAM_PIN) + '.')
#     else:
#         dt = datetime.now()
#         loggin.info('something: ' + str(dt))
#         # Logging is not working at all, nothing is written
#         # logging.info('IRBB activity detected')
#         # logging.INFO('IRBB activity detected at: ' + f"{dt:%H:%M:%S.%f}")
#         # This finally prints a .csv but the callback isn't working
#         csv_writer(str(box_id), 'IRBB', irbb_data, f"{dt.year}_{dt.month}_{dt.day}",
#                    header, [box_id, sensor_id, f"{dt.year}", f"{dt.month}", f"{dt.day}", f"{dt:%H:%M:%S.%f}"])
#         # TODO implement video function
#
#
# # Handler function for manual Ctrl + C cancellation
# def signal_handler(sig, frame):
#     GPIO.cleanup()
#     sys.exit(0)
#
# # TODO test: helper.email_alert('gsvidaurre@gmail.com', box + '_' + module, msg)
