# Created by PyCharm
# Author: nmoltta, gsvidaurre
# Project: ParentalCareTracking
# Date: 11/16/2021

# Import calls can stay in the script per device
import RPi.GPIO as GPIO

def detect_beam_breaks(BEAM_PIN):
    # beam not broken
    if GPIO.input(BEAM_PIN):
        return False
    # beam broken
    else:
        return True

# I think this belongs in the master script
#GPIO.setmode(GPIO.BCM)
#GPIO.setup(BEAM_PIN, GPIO.IN, pull_up_down=GPIO.PUD_UP)
#GPIO.add_event_detect(BEAM_PIN, GPIO.BOTH, callback=detect_beam_breaks)
#GPIO.cleanup()
