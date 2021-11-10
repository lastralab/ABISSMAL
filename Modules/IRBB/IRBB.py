# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 11/7/21

# G. Smith-Vidaurre 10 Nov 2021:
# Modified from https://simonprickett.dev/using-a-break-beam-sensor-with-python-and-raspberry-pi/
# This initial code works but needs to be modified to get time the beam was broken, and save these per day
# I'd like 2 beam breakers per nest container, but first need to get the code working for a single sensor

import RPi.GPIO as GPIO
import time
from datetime import datetime

# beam_pin = numeric; GPIO pin to which the receiver for the given sensor is connected
# sensor_id = string; "lead" (furthest from nest chamber) or "rear" (closest to nest chamber)
def detect_beam_breaks(beam_pin, sensor_id):

    # GPIO pin ID through which IR receiver transmits data
    BEAM_PIN = beam_pin

    def break_beam_callback(channel):
        if not GPIO.input(BEAM_PIN):

            dt = datetime.now()
            
            return{'sensor_id': sensor_id, 'event': "beam broken", 'year': "{}".format(dt.year),
                   'month': "{}".format(dt.month), 'day': "{}".format(dt.day), 'timestamp': "{}:{}:{}".format(dt.hour, dt.minute, dt.second)}

        
        
    GPIO.setmode(GPIO.BCM)
    GPIO.setup(BEAM_PIN, GPIO.IN, pull_up_down=GPIO.PUD_UP)
    GPIO.add_event_detect(BEAM_PIN, GPIO.BOTH, callback=break_beam_callback)

    # message = input("Press enter to quit\n\n")
    GPIO.cleanup()