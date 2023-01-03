# Created by PyCharm
# Author: nmoltta
# Project: Abissmal
# Date: 2/20/22

import subprocess
import time
from helper import sms_alert
from helper import dir_setup
from helper import get_logger
from helper import modules as mods
import datetime

dir_setup('/home/pi/')
modules = []

for module in mods:
    if module == 'Video':
        modules.append('video')
    if module == 'IRBB':
        modules.append('irbb')
    if module == 'RFID':
        modules.append('rfid')
    if module == 'Temp':
        modules.append('temp')

modules.append('backup')

logging = get_logger(datetime.date.today())
logging.info('Starting Monitor script')
print('Started Monitor script')


def monitor_screens():
    global modules
    screens = str(subprocess.getoutput("screen -list"))
    for screen in modules:
        if screens.find(screen) == -1:
            modules.remove(screen)
            sms_alert('Monitor', 'Error: Screen "' + screen + '" not running.')
            logging.error('Screen not running: ' + screen)
            print('Screen closed: ' + screen)
        else:
            pass
        

try:
    while True:
        monitor_screens()
        logging.info("Monitored screens")
        time.sleep(300)
        pass
except Exception as E:
    logging.error('Monitor error: ' + str(E))
    sms_alert('Monitor', 'Error while monitoring screens: ' + str(E))
    print('Monitor error: ' + str(E))
    pass
