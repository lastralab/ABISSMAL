# Created by PyCharm
# Author: nmoltta, gsvidaurre
# Project: ParentalCareTracking
# Date: 11/13/21

import picamera
import time
from datetime import datetime
import os

# This script contains code to record video for a given period of time. Another script will run this code and save output to a log file
# The second script will be called by cron at specific times of day each day over the experimental period

# Initialize metadata to be used for saving videos
chamber = "RC_01"
now = datetime.now()
out_path = "/home/pi/Desktop/videos"
file_name = os.path.join(out_path, f"{chamber}_{now.year}_{now.month}_{now.day}_{now.hour}_{now.minute}_{now.second}.h264")

# Initialize a log file that will contain the printed outout and will be named in a similar fashion as above
log_file_name = os.path.join(out_path, f"{chamber}_{now.year}_{now.month}_{now.day}_{now.hour}_{now.minute}_{now.second}.log")
f = open(os.path.join(out_path, log_file_name), "a")

# Perform video recording for a given duration of time
camera = picamera.PiCamera()

print("recording\n", file = f)
print(int(str(datetime.now().hour) + str(datetime.now().minute).zfill(2)), file = f)
print("\n", file = f)

# Reasonable iso values are 100 and 200 for daytime, 400 and 800 for low light
camera.iso = 400
# 1280x720 resolution is a 16:9 wide-screen aspect, looks pixelated
# anything x 1080 throws an error
# 1024x768 is a 4:3 aspect ratio
# Will need to play around with this to set width and height with respect to how high cameras are above birds once in cages
camera.resolution = (1000, 1000)
camera.framerate = 60
camera.rotation = 180
camera.start_recording(file_name)
# camera.wait_recording(60 * 30) # record for 30 minutes while checking for errors
camera.wait_recording(60*2) # testing with 60 second video
camera.stop_recording()

print("done video recording\n", file = f)
print(int(str(datetime.now().hour) + str(datetime.now().minute).zfill(2)), file = f)
print("\n", file = f)

# Close the file connection used to write output from print statements
f.close()
