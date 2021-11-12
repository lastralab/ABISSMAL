#!/usr/bin/env python3

"""  
Example code on accessing the RFID Reader 125kHz and reading data from tags

The code here is experimental, and is not intended to be used
in a production environment. It demonstrates the basics of what is
Required to get the Raspberry Pi receiving RFID data and configuring
the RFID Reader parameters.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation as version 2 of the License.

For more informaiton refer to www.cogniot.eu

Available commands:
  z - Display firmware version information
  S - Acknowledge presence of Tag
  F - Perform a Factory Reset
  P - Program EEPROM Polling delay
  v - Select reader operating mode
  R - Read Tag and PAGE 00 data
  r - Read Tag and BLOCK 04 data
  W - Write Tag and PAGE of data
  w - Write Tag and BLOCK of data
  A - Read All Pages 0 - 3f
  a - Read All blocks 0 - 16
  T - Test Mode (can be run using command line argument --testmode)
  e - Exit program


  
"""

import wiringpi as wiringpi2
import time
import sys
import argparse
import numbers

# set for GPIO Pin to use based on the jumper connection
GPIO_PIN = 1 # Jumper 1, also known as GPIO18
# GPIO_PIN = 0 # Jumper 2, also known as GPIO17
# GPIO_PIN = 2 # Jumper 3, also known as GPIO21 (Rv 1) or GPIO27 (Rv 2)
# GPIO_PIN = 3 # Jumper 4, also known as GPIO22


def WaitForCTS():
    # continually monitor the selected GPIO pin and wait for the line to go low
    # print ("Waiting for CTS")     # Added for debug purposes
    while wiringpi2.digitalRead(GPIO_PIN):
        # do nothing
        time.sleep(0.001)
    return

def ReadText(fd):
    # read the data back from the serial line and return it as a string to the calling function
    qtydata = wiringpi2.serialDataAvail(fd)
    # print ("Amount of data: %d bytes" % qtydata)   # Added for debug purposes
    response = ""
    while qtydata > 0:
        # while there is data to be read, read it back
        # print ("Reading data back %d" % qtydata)   #Added for Debug purposes
        response = response + chr(wiringpi2.serialGetchar(fd))
        qtydata = qtydata - 1   
    return response

def ReadInt(fd):
    # read a single character back from the serial line
    qtydata = wiringpi2.serialDataAvail(fd)
    # print ("Amount of data: %s bytes" % qtydata)    # Added for debug purposes
    response = 0
    if qtydata > 0:
        # print ("Reading data back %d" % qtydata)   #Added for Debug purposes
        response = wiringpi2.serialGetchar(fd)
    return response

def RFIDSetup():
    # setup up the serial port and the wiringpi software for use
    # call setup for the wiringpi2 software
    response = wiringpi2.wiringPiSetup()
    # set the GPIO pin for input
    wiringpi2.pinMode(GPIO_PIN, 0)
    # open the serial port and set the speed accordingly
    fd = wiringpi2.serialOpen('/dev/serial0', 9600)

    # clear the serial buffer of any left over data
    wiringpi2.serialFlush(fd)
    
    if response == 0 and fd >0:
        # if wiringpi is setup and the opened channel is greater than zero (zero = fail)
        print ("PI setup complete on channel %d" %fd)
    else:
        print ("Unable to Setup communications")
        sys.exit()
        
    return fd

def ReadVersion(fd):
    # read the version from the RFID board
    WaitForCTS()
    # print ("Sending Read Version Command")     #Added for Debug purposes
    wiringpi2.serialPuts(fd,"z")
    time.sleep(0.1)
    ans = ReadText(fd)
    print ("Response: %s" % ans)

def ReadTagStatus(fd):
    # read the RFID reader until a tag is present
    notag = True
    while notag:
        WaitForCTS()
        # print ("Sending Tag Status Command")   #Added for Debug purposes
        wiringpi2.serialPuts(fd,"S")
        time.sleep(0.1)
        ans = ReadInt(fd)
        # print ("Tag Status: %s" % hex(ans))    # Added for Debug purposes
        if ans == int("0xD6", 16):
            # D6 is a positive response meaning tag present and read
            notag = False
    print ("Tag Status: %s" % hex(ans))
    return

def TagPresent(fd):
    # read the RFID reader until a tag is present
    notag = True
    while notag:
        WaitForCTS()
        # print ("Sending Tag Status Command")   #Added for Debug purposes
        wiringpi2.serialPuts(fd,"S")
        time.sleep(0.1)
        ans = ReadInt(fd)
        # print ("Tag Status: %s" % hex(ans))    # Added for Debug purposes
        if ans == int("0xD6", 16):
            # D6 is a positive response meaning tag present and read
            notag = False
    print ("\nTag Present\n")
    return
    
def FactoryReset(fd):
    # send the factory reset command
    WaitForCTS()
    # print ("Performing a factory reset ....") #Added for Debug purposes
    wiringpi2.serialPutchar(fd, 0x46)
    wiringpi2.serialPutchar(fd, 0x55)
    wiringpi2.serialPutchar(fd, 0xAA)
    time.sleep(0.1)
    print ("FACTORY RESET COMPLETE ")
    print ("")
    return

def SetPollingDalay(fd):
    # set the polling delay for the reader
    print ("Setting Polling delay .......")
    WaitForCTS()

    wiringpi2.serialPutchar(fd, 0x50)
    wiringpi2.serialPutchar(fd, 0x00)
    # various polling delays possible, standard one uncommented
    #wiringpi2.serialPutchar(fd, 0x00) # 0x00 is no delay
    #wiringpi2.serialPutchar(fd, 0x20) # 0x20 is approx 20ms
    #wiringpi2.serialPutchar(fd, 0x40) # 0x40 is approx 65ms
    wiringpi2.serialPutchar(fd, 0x60) # 0x60 is approx 262ms
    #wiringpi2.serialPutchar(fd, 0x80) # 0x60 is approx 1 Seconds
    #wiringpi2.serialPutchar(fd, 0xA0) # 0x60 is approx 4 Seconds

    time.sleep(0.1)
    ans = ReadInt(fd)
    # print ("Tag Status: %s" % hex(ans))    # Added for Debug Purposes 
    if ans == int("0xC0", 16):
        # C0 is a positive result
        print ("Polling delay changed ......")
    else:
        print ("Unexpected response %s" % hex(ans))
        # flush any remaining characters from the buffer
        wiringpi2.serialFlush(fd)
    return

def ReadTagPageDefaultZero(fd,page=00):
    # read the tag page 00 command
    notag = True

    print ("\nReading Tag Data Page %d......." % page)

    print ("\nWaiting for a tag ....")

    notag = True
    while notag:
        WaitForCTS()
        # print ("Sending Tag Read Page Command")    #Added for Debug purposes
        wiringpi2.serialPutchar(fd, 0x52)
        wiringpi2.serialPutchar(fd, page)
        time.sleep(0.1)
        ans = ReadInt(fd)
        # print ("Tag Status: %s" % hex(ans))    #Added for Debug purposes
        if ans == int("0xD6", 16):
            # Tag present and read
            notag = False
            # print ("Tag Present") #Added for Debug purposes
            ans = ReadText(fd)
            print ("\nPage %d" % page)
            print ("-->%s<--" % ans)
            print("++>", end="")
            for f in ans:
                print(" %d" % ord(f), end="")
            print(" <++")
    return

def ReadAllPages(fd):
    # Cycle through and read all pages available

    print("Reading all Pages")
    for f in range (0,0x3f):
        ReadTagPageDefaultZero(fd, f)
    return

def ReadTagAndBlocks(fd, block=4, enter=False):
    # read the tag and all blocks from within it
    # Only works for HS/1 as other tags don't support it
    notag = True

    if enter:
        # enter flag determines if user is prompted for an entry
        block = CaptureBlockPageNo()
    
    print ("\nReading Tag Data Block %x ......." % block)

    print ("\nWaiting for a tag ....")

    notag = True
    while notag:
        WaitForCTS()
        # print ("Sending Tag Read Blocks command")  #Added for Debug purposes
        wiringpi2.serialPutchar(fd, 0x72)
        wiringpi2.serialPutchar(fd, block)
        time.sleep(0.1)
        ans = ReadInt(fd)
        # print ("Tag Status: %s" % hex(ans))    #Added for Debug purposes
        if ans == int("0xD6", 16):
            # Tag present and read
            notag = False
            #print ("Tag Present")  #Added for Debug purposes
            ans = ReadText(fd)
            print ("\nBlocks %x" % block)
            print ("-->%s<--" % ans)
            print("++>", end="")
            for f in ans:
                print(" %d" % ord(f), end="")
            print(" <++")
    return

def ReadAllBlocks(fd):
    #read throught all the blocks

    print("Reading all blocks")
    for f in range (0, 16):
        ReadTagAndBlocks(fd,f, False)
    return
  

def CaptureDataToWrite(qty):
    # provide an additional menu to capture the data to be written to the tag
    # returns the bytes to be written
    
    to_write = []
    choice = ""
    counter = 0
    print ("*********************************************")
    print ("Please enter %d bytes of data to be written" % qty)

    while (counter < qty):
        # Loop round getting, checking and saving the byte
        # promt the user for a choice
        choice = input("Please enter byte %d to write (0 - 255).....:" % counter)
        try:
            # value entered is a number, so process it
            # print ("Values read:%d" % int(choice))            # added for debug purposes
            if int(choice) > 0 and int(choice) < 255:
                to_write.append(int(choice))
                counter = counter + 1
                # print("Value Captured")           # added for debug purposes
            else:
                print("Please ensure the number is between 0 and 255")
        except:
            print("Please ensure you enter a number between 0 and 255")

    # print ("data caltured:%s" % to_write)  # Added for Debug purposes
    
    return to_write

def CaptureBlockPageNo():
    # provide an additional menu to capture the block or page number
    # returns the block / page to be written
    
    to_write = 0
    choice = ""
    success = False 
    print ("*********************************************")
    print ("Please enter the block / page to be written")

    while (success == False):
        # Loop round getting, checking and saving the byte
        # promt the user for a choice
        choice = input("Please enter block / page to write.....:")
        try:
            # value entered is a number, so process it
            # print ("Value read:%d" % int(choice))            # added for debug purposes
            if int(choice) > 0 and int(choice) < 255:
                to_write = int(choice)
                success = True
                # print("Value Captured")           # added for debug purposes
            else:
                print("Please ensure the number is between 0 and 255")
        except:
            print("Please ensure you enter a number between 0 and 255")

    # print ("data captured:%s" % to_write)  # Added for Debug purposes
    
    return to_write
    
def WriteTagPage(fd):
    # write to the tag page, user selecting the page
    notag = True
    block = []
    pagesize = 4

    page = CaptureBlockPageNo()

    block = CaptureDataToWrite(pagesize)

    print ("\nWriting Tag Data Page %s......." % page)
    
    #print("Data to write:%s" % block)         #Added for Debug purposes
    
    print ("\nWaiting for a tag ....")

    notag = True
    while notag:
        WaitForCTS()
        #print ("Sending Tag Write Page Command")    #Added for Debug purposes
        wiringpi2.serialPutchar(fd, 0x57)
        wiringpi2.serialPutchar(fd, page)           # Write to page four
        wiringpi2.serialPutchar(fd, block[0])
        wiringpi2.serialPutchar(fd, block[1])
        wiringpi2.serialPutchar(fd, block[2])
        wiringpi2.serialPutchar(fd, block[3])
        time.sleep(0.1)
        ans = ReadInt(fd)
        #print ("Tag Status: %s" % hex(ans))    #Added for Debug purposes
        if ans == int("0xD6", 16):
            # Tag present and read
            notag = False
            #print ("Tag Present") #Added for Debug purposes
            ReadTagPageDefaultZero(fd,page)
    return

def WriteTagAndBlocks(fd):
    # write the tag and all blocks from within it
    # Only works for HS/1 as other tags don't support it
    notag = True
    block = []
    blockno = 0
    blocksize = 16

    blockno = CaptureBlockPageNo()
    block = CaptureDataToWrite(blocksize)

    print ("\nWriting Tag Data Block %d ......." % blockno)

    # print("Data to write:%s" % block)         #Added for Debug purposes

    print ("\nWaiting for a tag ....")

    notag = True
    while notag:
        WaitForCTS()
        #print ("Sending Tag Write Blocks command and data")  #Added for Debug purposes
        wiringpi2.serialPutchar(fd, 0x72)
        wiringpi2.serialPutchar(fd, blockno)
        for f in block:
            wiringpi2.serialPutchar(fd, f)
        time.sleep(0.1)
        ans = ReadInt(fd)
        #print ("Tag Status: %s" % hex(ans))    #Added for Debug purposes
        if ans == int("0xD6", 16):
            # Tag present and read
            notag = False
            #print ("Tag Present")  #Added for Debug purposes
            ans = ReadText(fd)

            ReadTagAndBlocks(fd, blockno, False)
    return

def SetReaderMode(fd, choice):
    # Given the mode choice, set the mode
    desc = ""
    if choice =="a" or choice == "A":
        desc = "Hitag H2"
        WaitForCTS()
        wiringpi2.serialPutchar(fd, 0x76)
        wiringpi2.serialPutchar(fd, 0x01) # 0x01 = H2
    elif choice =="b" or choice == "B":
        desc = "Hitag H1/S"
        WaitForCTS()
        wiringpi2.serialPutchar(fd, 0x76)
        wiringpi2.serialPutchar(fd, 0x02) # 0x01 = H1/S
    elif choice =="c" or choice == "C":
        desc = "Em / MC2000"
        WaitForCTS()
        wiringpi2.serialPutchar(fd, 0x76)
        wiringpi2.serialPutchar(fd, 0x03) # 0x03 = EM/MC2000
    else:
        print ("Invalid option.\n")
        choice = ""
        return

    time.sleep(0.1)
    ans = ReadInt(fd)
    print ("Tag Status: %s" % hex(ans)) #Added for Debug purposes
    if ans == int("0xC0", 16):
        # Positive result
        print ("Reader Operating Mode %s ......" % desc)
    else:
        print ("Unexpected response %s" % hex(ans))
        # clear the buffer
        wiringpi2.serialFlush(fd)
    return
    
def UserChangeReaderOpMode(fd):
    # prvide an additional menu to choose the type of tag to be read and set the reader accordingly
    print ("Setting Reader Operating Tag Mode.......\n")

    choice = ""
    print ("*********************************************")
    print ("a - Hitag H2")
    print ("b - Hitag H1/S (factory default)")
    print ("c - EM/MC2000\n\n")
    # promt the user for a choice
    choice = input("Please select tag type .....:")
    # print ("choice: %s" % choice)  # Added for Debug purposes
    
    SetReaderMode(fd,choice)
    return

def TestMode(fd):
    """ Routine to check the RFID module works
    Runs the following checks
    - Press any key to continue
    - Verify board type
    - Select tag mode A
        - Check tag range
    - Repeat for modes B and C
    - Set to default mode C
        - any key to change """
    key = input("\nHit ENTER to Start test ...")
    #ReadVersion(fd)
    for modes in ("B","C"):
        print("Present tag of mode %s" % modes)
        SetReaderMode(fd,modes)
        TagPresent(fd)
    key = input("\nHit ENTER to set back to default mode,\n else press A,B or C & Enter to set specific mode")
    if key.upper() in ("A","B","C"):
        print("\nSetting Mode :%s\n" % key.upper())
        FactoryReset(fd)
        SetReaderMode(fd,key.upper())
    else:
        print("\nSetting Default (C) Mode\n")
        FactoryReset(fd)
        SetReaderMode(fd,"C")
    return
    
def ArgsParser():
    # Setup and read the arguments back from the command line
    parser = argparse.ArgumentParser()
    parser.add_argument("--testmode", help="Run in Test Mode", action="store_true")
    args = parser.parse_args()
    return args.testmode
    
def HelpText():
    # show the help text
    print ("**************************************************************************\n")
    print ("Available commands: -")
    print ("z - Display firmware version information")
    print ("S - Acknowledge presence of Tag")
    print ("F - Perform a Factory Reset")
    print ("P - Program EEPROM Polling delay")
    print ("v - Select reader operating mode")
    print ("R - Read Tag and PAGE 00 data")
    print ("r - Read Tag and BLOCK 04 data")
    print ("W - Write Tag and PAGE of data")
    print ("w - Write Tag and BLOCK of data")
    print ("A - Read All Pages 0 - 3f")
    print ("a - Read All blocks 0 - 16")
    print ("T - Test Mode")
    print ("e - Exit program\n\n")



# main code loop

print ("Bostin Technology Ltd")
print ("Cogniot Products")
print ("PirFlx")
print ("")
print ("Press h for help")
print ("")

comms = RFIDSetup()

test_mode = ArgsParser()
if test_mode:
    print("\n\nEntering Test Mode\n")
    time.sleep(0.5)
    TestMode(comms)
    sys.exit(1)


HelpText()

while True:
    choice = input ("Select Menu Option:")

    if choice == "H" or choice == "h":
        HelpText()
    elif choice == "z":
        ReadVersion(comms)
    elif choice == "S":
        ReadTagStatus(comms)
    elif choice == "F":
        FactoryReset(comms)
    elif choice == "P":
        SetPollingDalay(comms)
    elif choice == "v":
        UserChangeReaderOpMode(comms)
    elif choice == "R":
        ReadTagPageDefaultZero(comms)
    elif choice == "r":
        ReadTagAndBlocks(comms)
    elif choice == "W":
        WriteTagPage(comms)
    elif choice == "w":
        WriteTagAndBlocks(comms)
    elif choice == "A":
        ReadAllPages(comms)
    elif choice == "a":
        ReadAllBlocks(comms)
    elif choice == "T":
        TestMode(comms)
    elif choice == "E" or choice == "e":
        sys.exit()


