# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 11/7/21

# G. Smith-Vidaurre 10 Nov 2021:
# Modified from https://simonprickett.dev/using-a-break-beam-sensor-with-python-and-raspberry-pi/
# This initial code works but needs to be modified to get time the beam was broken, and save these per day
# I'd like 2 beam breakers per nest container, but first need to get the code working for a single sensor
# Lead sensor is the first sensor from the tunnel entrance (e.g. from outside into nest chamber)
# Rear sensor will be behind the lead, the closest to the nest chamber entrance)
#sensor_ID = "lead" 

import RPi.GPIO as GPIO

# GPIO pin ID through which IR receiver transmits data
BEAM_PIN = 16

def break_beam_callback(channel):
    if GPIO.input(BEAM_PIN):
        print("beam unbroken")
    else:
        print("beam broken")
        
GPIO.setmode(GPIO.BCM)
GPIO.setup(BEAM_PIN, GPIO.IN, pull_up_down=GPIO.PUD_UP)
GPIO.add_event_detect(BEAM_PIN, GPIO.BOTH, callback=break_beam_callback)

message = input("Press enter to quit\n\n")
GPIO.cleanup()