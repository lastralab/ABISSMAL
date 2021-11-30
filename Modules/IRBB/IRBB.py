# Created by PyCharm
# Author: nmoltta, gsvidaurre
# Project: ParentalCareTracking
# Date: 11/16/2021

# Previous code
# Import calls need to be here and in master script otherwise code fails
import RPi.GPIO as GPIO
import time
from datetime import datetime

def detect_beam_breaks_callback(BEAM_PIN):
    # Return date and timestamp when the beam is broken
    if not GPIO.input(BEAM_PIN):
        dt = datetime.now()
        print(f'{dt:%Y-%m-%d %H:%M:%S.%f}')
        
# Handler function for manual Ctrl + C cancellation
def signal_handler(sig, frame):
    GPIO.cleanup()
    sys.exit(0)