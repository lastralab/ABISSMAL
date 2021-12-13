# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 11/7/21

#!/usr/bin/env python3

import wiringpi as wiringpi2
import numpy as np
import time
import sys
from datetime import datetime

#set GPIO pin
GPIO_PIN = 1 # GPIO18
# GPIO_PIN = 0 # GPIO17
# GPIO_PIN = 2 # GPIO21
# GPIO_PIN = 3 #GPIO22

#wait for input from pin
def WaitForCTS():
    while wiringpi2.digitalRead(GPIO_PIN):
        time.sleep(0.001)
    return

#initiate and open communication with RFID reader
def RFIDSetup():
    response = wiringpi2.wiringPiSetup()
    wiringpi2.pinMode(GPIO_PIN, 0)
    fd = wiringpi2.serialOpen('/dev/serial0', 9600)
    wiringpi2.serialFlush(fd)
    if response != 0 and fd <= 0:
        print ("Unable to Setup communications")
        sys.exit()
    return fd

#set reader mode to EM4102 compatible
def SetReaderMode(fd):
    WaitForCTS()
    wiringpi2.serialPutchar(fd, 0x76)
    wiringpi2.serialPutchar(fd, 0x03) #EM/MC2000

#set polling delay to 20 ms
def SetPollingDelay(fd):
    WaitForCTS()
    wiringpi2.serialPutchar(fd, 0x50)
    wiringpi2.serialPutchar(fd, 0x60) #262 ms

#read text from RFID reader
def ReadText(fd):
    response = ""
    i = 1
    while i <= 5:
        temp = format(wiringpi2.serialGetchar(fd), 'x').upper()
        if len(temp) == 1:
            temp = '0' + temp
        if i <= 4:
            temp = temp + "-"
        response = response + temp
        i = i + 1
    return response

#read int from RFID reader
def ReadInt(fd):
    qtydata = wiringpi2.serialDataAvail(fd)
    response = 0
    if qtydata > 0:
        response = wiringpi2.serialGetchar(fd)
    return response

#read RFID tag
def ReadTagPageZero(fd):
    notag = True
    while notag:
        WaitForCTS()
        wiringpi2.serialPutchar(fd, 0x52)
        wiringpi2.serialPutchar(fd, 0x00)
        time.sleep(0.1)
        ans = ReadInt(fd)
        if ans == int("0xD6", 16):
            notag = False
            ans = ReadText(fd)
            print("%s %s %s" % (ans, datetime.now().date().strftime("%m/%d/%y"), datetime.now().time().strftime("%H:%M:%S")))
            notag = True

comms = RFIDSetup()
SetReaderMode(comms)
SetPollingDelay(comms)
ReadTagPageZero(comms)


