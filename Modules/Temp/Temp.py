# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 11/7/21

# G. Smith-Vidaurre
# Updated 01 Nov 2021
# Modified from https://pimylifeup.com/raspberry-pi-temperature-sensor/
# Note that sensor must be connected to 3.3V for power and pin #4 for 1-wire data transfer
# A single sub-folder per device should appear inside /sys/bus/w1/devices that starts with 28 after initial setup (see link)
# Otherwise something is not connected correctly (e.g. multiple folders that start with 00-)

import os
import glob
import time
from datetime import datetime
import csv
from pathlib import Path

os.system('modprobe w1-gpio')
os.system('modprobe w1-therm')

base_dir = '/sys/bus/w1/devices/'
device_folder = glob.glob(base_dir + '28*')[0]
device_file = device_folder + '/w1_slave'

# To run this script execute with python3, otherwise the file = f statement in print calls will fail
# Also had to rerun setup of /w1 folder as in https://pimylifeup.com/raspberry-pi-temperature-sensor/

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
        #return "{} (degrees Celsius); {} degrees Farenheit".format(temp_c, temp_f)

# The while loop is always TRUE and therefore will run forever, unless there's an erorr or the user interrupts the run
# The script is put to sleep every 60 seconds, so the temperature will print every minute
# Print to a file instead of the screen. With date and time
file_name = "RC_01_temperature"
out_path = "/home/pi"
pathfile = os.path.join(out_path, f"{file_name}.csv")

# If the file does not already exist, then write column headers to the .csv
csvfile = Path(pathfile)
#f = open(pathfile, "a", newline = "")
#writer = csv.writer(f, delimiter = ",")

if not csvfile.is_file():
    header = [
        ['recording_chamber', 'year', 'month', 'day', 'time', 'degrees_Celsius', 'degrees_Farenheit']
    ]  
    f = open(pathfile, "a", newline = "")
    writer = csv.writer(f, delimiter = ",")
    writer.writerows(header)

while True:
    
    dt = datetime.now()
    temp = read_temp()

    # Print output to a csv
    tmp_row = [
            ['RC_01', "{}".format(dt.year), "{}".format(dt.month), "{}".format(dt.day), "{}:{}:{}".format(dt.hour, dt.minute, dt.second), temp[0], temp[1]]
    ]

    f = open(pathfile, "a", newline = "")
    writer = csv.writer(f, delimiter = ",")
    writer.writerows(tmp_row)

    time.sleep(60)

