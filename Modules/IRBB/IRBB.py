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


def detect_beam_breaks_callback():
    # Return date and timestamp when the beam is broken
    if not GPIO.input(BEAM_PIN):
        dt = datetime.now()
        print(dt)


# Handler function for manual Ctrl + C cancellation
def signal_handler(sig, frame):
    GPIO.cleanup()
    sys.exit(0)


timestamp = GPIO.add_event_detect(BEAM_PIN, GPIO.FALLING, callback=detect_beam_breaks_callback,
                                  bouncetime=100)
if not timestamp:
    logging.info('No IRBB activity on pin ' + BEAM_PIN + '.')
else:
    logging.info('IRBB activity detected.')
    if isinstance(timestamp, date):
        # TODO start video, record for 1 minute...
        helper.csv_writer(box_id, module, date.today(), timestamp)
    else:
        warn = warn + 1
        msg = 'IRBB is not returning a timestamp: ' + timestamp
        logging.error(msg) if warn > 3 else logging.warn(msg)
        if warn > 3:
            warn = 0
        # TODO test: helper.email_alert('gsvidaurre@gmail.com', box + '_' + module, msg)
