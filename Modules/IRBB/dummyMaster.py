# Created in vim
# Author: gsvidaurre
# Project: ParentalCareTracking
# Date: 11/16/2021

# Purpose: A dummy master script to test the function in IRBB.py. This dummy script should pass parameters specific to the given recording chamber to the function in IRBB.py. Will use this dummy script to test the updated IRBB.py code on a Raspberry Pi

# Import function in IRBB.py
import IRBB

# GPIO pin ID through which IR receiver transmits data
BEAM_PIN = 16

# Run the code to set up the GPIO pin for IR event detec tion
GPIO.setmode(GPIO.BCM)
GPIO.setup(BEAM_PIN, GPIO.IN, pull_up_down = GPIO.PUD_UP)

# Save the output of the beam break detection function to an object
irbb_events = GPIO.add_event_detect(BEAM_PIN, GPIO.BOTH, callback = detect_beam_breaks)

# Print this output
# I think a problem here is that the output is continuous, and I'm only interested in the True values that indicate that the beam was broken
# Print irbb_events only if True. How quickly will this update?
if(irbb_events):
    print(irbb_events)

# Clean up the GPIO pin after IR detection is done
GPIO.cleanup()
