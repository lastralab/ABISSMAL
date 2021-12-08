import time
import signal
from datetime import datetime
import sys
import RPi.GPIO as GPIO
import logging
from helper import logger_setup
from helper import csv_writer
from helper import box_id
from helper import irbb_data
from os.path import exists
import csv

BEAM_PIN = 16
warn = 0
module = 'IRBB'

# CSV header
header = ['chamber_id', 'sensor_id', 'year', 'month', 'day', 'timestamp']
sensor_id = "lead"

dt = datetime.now()
        
csv_writer(str(box_id), 'IRBB', irbb_data, f"{dt.year}_{dt.month}_{dt.day}",
           header, [box_id, sensor_id, f"{dt.year}", f"{dt.month}", f"{dt.day}",
                    f"{dt:%H:%M:%S.%f}"])


date = f"{dt.year}_{dt.month}_{dt.day}"
value = [box_id, sensor_id, f"{dt.year}", f"{dt.month}", f"{dt.day}",
                    f"{dt:%H:%M:%S.%f}"]
data_path = irbb_data
filename = module + '_' + str(box_id) + '_' + date + '.csv'
full_path = data_path + '/' + filename
if exists(full_path):
    file = open(full_path, 'a+')
    #file.write(value)
    tmp_writer = csv.writer(file)
    tmp_writer.writerow(header)
    tmp_writer.writerow(value)
    file.close()
else:
    file = open(full_path, 'w+')
    #file.write(header)
    #file.write(value)
    tmp_writer = csv.writer(file)
    tmp_writer.writerow(header)
    tmp_writer.writerow(value)
    file.close()


