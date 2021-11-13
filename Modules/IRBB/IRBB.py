# Created by PyCharm
# Author: nmoltta
# Contributor: G. Smith-Vidaurre
# Project: ParentalCareTracking
# Date: 11/7/21

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
