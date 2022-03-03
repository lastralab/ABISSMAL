# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 2/20/22

import subprocess
import time
from helper import email_alert
from helper import dir_setup
from helper import get_logger
import datetime

dir_setup('/home/pi/')
modules = ['temp', 'video', 'rfid', 'irbb', 'backup']

logging = get_logger(datetime.date.today())
logging.info('Starting Monitor script')
print('Starting Monitor script')


def monitor_screens(today):
    logging = get_logger(today)
    screens = str(subprocess.getoutput("screen -list"))
    for screen in modules:
        if screens.find(screen) == -1:
            email_alert(screen, 'Screen not running.')
            logging.error('Screen not running: ' + screen)
            print('Screen closed: ' + screen)
        else:
            pass


try:
    while True:
        monitor_screens(datetime.date.today())
        logging.info("Monitored screens")
        time.sleep(300)
        pass
except Exception as E:
    logging.error('Monitor error: ' + str(E))
    email_alert('Monitor', 'Error while monitoring screens: ' + str(E))
    print('Monitor error: ' + str(E))
    pass
