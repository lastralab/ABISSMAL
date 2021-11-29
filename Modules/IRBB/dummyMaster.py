# Created in vim
# Author: gsvidaurre
# Project: ParentalCareTracking
# Date: 11/16/2021

# Purpose: A dummy master script to test the function in IRBB.py. This dummy script should pass parameters specific to the given recording chamber to the function in IRBB.py. Will use this dummy script to test the updated IRBB.py code on a Raspberry Pi
import signal # for Ctrl+C handling
import sys # for Ctrl+C handling, get error that this is not defined when hitting "Ctrl + C"
import RPi.GPIO as GPIO
import time
from datetime import datetime

# Import callback and handler functions in IRBB.py
from IRBB import detect_beam_breaks_callback
from IRBB import signal_handler

# GPIO pin ID through which IR receiver transmits data
BEAM_PIN = 16

# Add a script guard for running modules....not sure if this should be included in master script
#if __name__ == '__main__':

# Run the code to set up the GPIO pin for IR event detection
GPIO.setmode(GPIO.BCM)

# Here, setting a pull up resistor
# This means that the default GPIO state is HIGH, and should change to LOW when the beam breaks
# Therefore, can use GPIO.FALLING below to detect edges
# Need to set up code to detect falling interrupts (HIGH to LOW)
# Avoid polling, since it's easy to miss edges this way
# Also, better to use add_event_detect() and callback(), since wait_for_edge apparently blocks the main thread and causes problems when trying to run other code at the same time
GPIO.setup(BEAM_PIN, GPIO.IN, pull_up_down = GPIO.PUD_UP)

# From GPIO documentation: "An edge is the name of a transition from HIGH to LOW (falling edge) or LOW to HIGH (rising edge)"
# Replaced GPIO.BOTH with GPIO.FALLING to detect only when beam is broken
# bouncetime parameter ensures that if beam breaking flickers rapidly then only a single detection will be returned

timestamp = GPIO.add_event_detect(BEAM_PIN, GPIO.FALLING, callback = detect_beam_breaks_callback,
                          bouncetime = 100)

if not timestamp:
    print("the variable is null")
else:
    print(timestamp)
    
    
# Register callback for manual cancellation of code by CTRL + C
# signal.pause() causes program to pause forever, so program doesn't exit right away
signal.signal(signal.SIGINT, signal_handler)
signal.pause()
