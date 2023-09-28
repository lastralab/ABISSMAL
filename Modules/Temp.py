# Created by PyCharm
# Author: tanismo, gsvidaurre
# Project: Abissmal
# Date: 12/13/21

import os
import glob
import time
from datetime import datetime
import csv
from pathlib import Path
from helper import dir_setup
from helper import csv_writer
from helper import box_id
from time import sleep
from helper import sms_alert
from helper import get_logger

dir_setup('/home/pi/')
logging = get_logger(datetime.today())
logging.info('Started Temp')
print('Started Temp')

warn = 0
module = 'Temp'
header = ['chamber_id', 'sensor_id', 'year', 'month', 'day', 'time', 'degrees_Celsius', 'degrees_Farenheit']
temp_data = '/home/pi/Data_Abissmal/Temp'
os.system('modprobe w1-gpio')
os.system('modprobe w1-therm')
base_dir = '/sys/bus/w1/devices/'
device_folder = glob.glob(base_dir + '28*')[0]
device_file = device_folder + '/w1_slave'


def read_temp_raw():
    f = open(device_file, 'r')
    lines = f.readlines()
    f.close()
    return lines


def read_temp():
    lines = read_temp_raw()
    if len(lines) > 1:
        while lines[0].strip()[-3:] != 'YES':
            time.sleep(0.25)
            lines = read_temp_raw()
        equals_pos = lines[1].find('t=')
        if equals_pos != -1:
            temp_string = lines[1][equals_pos + 2:]
            temp_c = float(temp_string) / 1000.0
            temp_f = (temp_c * (9.0 / 5.0)) + 32.0
            return temp_c, temp_f
    else:
        return None


try:
    while True:
        dt = datetime.now()
        temp = read_temp()
        if temp is not None:
            C = str(round(temp[0], 2))
            F = str(round(temp[1], 2))
            logging.info('Temperature reading: ' + C + u'\N{DEGREE SIGN}' + 'C, ' + F + u'\N{DEGREE SIGN}' + 'F')
            print('Temperature registered')
            csv_writer(str(box_id), module, temp_data, dt.strftime("%Y_%m_%d"), header,
                       [box_id, 'Temperature', dt.strftime("%Y"), dt.strftime("%m"), dt.strftime("%d"), f"{dt:%H:%M:%S.%f}", temp[0], temp[1]])
            time.sleep(60)
        else:
            C = str("N/A")
            F = str("N/A")
            logging.info('Temperature reading: N/A')
            sms_alert('Temp', 'Warning: Sensor reading returned N/A, run cron.sh if it you get this warning again.')
            print('Temperature registered as N/A')
            csv_writer(str(box_id), module, temp_data, dt.strftime("%Y_%m_%d"), header,
                       [box_id, dt.strftime("%Y"), dt.strftime("%m"), dt.strftime("%d"), f"{dt:%H:%M:%S.%f}", C, F])
            time.sleep(60)
except KeyboardInterrupt:
    logging.info('Exiting Temperature')
except Exception as E:
    logging.error('Temperature error: ' + str(E))
    print('Temperature error: ' + str(E))
    sms_alert('Temperature', 'Error: ' + str(E))
