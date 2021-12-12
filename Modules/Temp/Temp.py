# Created by PyCharm
# Author: nmoltta, gsvidaurre
# Project: ParentalCareTracking
# Date: 11/13/21

import os
import glob
import time
from datetime import datetime
import csv
from pathlib import Path

# For logging and writing to a .csv file
from helper import logger_setup
from helper import csv_writer
from helper import box_id
from time import sleep

logger_setp('/home/pi')

warn = 0
module = 'Temp'

# CSV header
header = ['chamber_id', 'year', 'month', 'day', 'time', 'degrees_Celsius', 'degrees_Farenheit']
temp_data = '/home/pi/Data_ParentalCareTracking/Temp'

logging.info('started temperature script')

# Set up temperature probe 1-wire data transfer
os.system('modprobe w1-gpio')
os.system('modprobe w1-therm')

base_dir = '/sys/bus/w1/devices/'
device_folder = glob.glob(base_dir + '28*')[0]
device_file = device_folder + '/w1_slave'


# Open file that contains temperature output
def read_temp_raw():
    f = open(device_file, 'r')
    lines = f.readlines()
    f.close()
    return lines

# Process data obtained from read_temp_raw
def read_temp():
    lines = read_temp_raw()
    # Make sure the first line contains 'YES', which means there is a temperature reading in the next line
    while lines[0].strip()[-3:] != 'YES':
        time.sleep(0.2)
        lines = read_temp_raw()
    # Find the line with the temperature reading
    # lines[1] is the second line from the raw output, or the 2nd element of the array of raw output
    equals_pos = lines[1].find('t=')
    if equals_pos != -1:
        # From the line with the temperature reading, get all numbers after the pattern "t=". equals_pos is the start position of the temperature, and by adding 2 to this, only the temperature value is returned
        # Convert temperature to Celsius and Farenheit
        temp_string = lines[1][equals_pos+2:]
        temp_c = float(temp_string) / 1000.0
        temp_f = (temp_c * (9.0 / 5.0)) + 32.0
        return temp_c, temp_f

while True:

    dt = datetime.now()
    temp = read_temp()

    logging.info('Temperature sensor reading at:' + f"{dt:%H:%M:%S.%f}")

    csv_writer(str(box_id), module, temp_data, f"{dt.year}_{dt.month}_{dt.day}", header, [box_id, f"{dt.year}", f"{dt.month}", f"{dt.day}", f"{dt:%H:%M:%S.%f}", temp[0], temp[1]])

    time.sleep(60)
