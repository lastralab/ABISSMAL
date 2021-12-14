import time
import signal
from datetime import datetime
import sys
import RPi.GPIO as GPIO
import logging
from helper import logger_setup
from helper import csv_writer
from helper import box_id
from os.path import exists
import csv
from picamera import PiCamera
from subprocess import call

video_data = "/home/pi/Data_ParentalCareTracking/Video/"


def convert(file_h264):

    command = "MP4Box -add " + file_h264 + " " + file_mp4
    call([command], shell=True)
    logging.info('Converted video h264 to mp4.')


# TODO test adding function to beam breaker function, passing time param
# TODO try catch, log errors
# TODO try recording with ring buffer:
# See https://gist.github.com/waveform80/8496879
# Also see picamera docs section 3.12 Recording to circular stream in https://picamerax.readthedocs.io/en/latest/recipes1.html
def record_video(path, box_id, dt_str):
    file_h264 = path + str(box_id) + dt_str + '.h264'
    camera = PiCamera()
    # Reasonable iso values are 100 and 200 for daytime, 400 and 800 for low light
    camera.iso = 400
    # 1280x720 resolution is a 16:9 wide-screen aspect, looks pixelated
    # Anything x 1080 throws an error 
    # 1024x768 is a 4:3 aspect ratio
    # Will need to play around with this to set width and height with respect to how high cameras are above birds once in cages
    camera.resolution = (640, 640) 
    camera.framerate = 60
    camera.start_recording(file_h264)
    camera.wait_recording(5)
    camera.stop_recording()
    file_mp4 = path + str(box_id) + dt_str + '.mp4'
    # MP4Box prints a lot of messages, could these be saved to the log via Main.sh?
    command = "MP4Box -add " + file_h264 + " " + file_mp4
    call([command], shell=True)
    logging.info('Converted video ' + file_h264 + ' to mp4.')
    
    
dt = datetime.now()
record_video(video_data, box_id, f"{dt.year}_{dt.month}_{dt.day}_{dt:%H}_{dt:%M}_{dt:%S}")

#"Camera is not enabled. Try running 'sudo raspi-config' "
#picamera.exc.PiCameraError: Camera is not enabled. Try running 'sudo raspi-config' and ensure that the camera has been enabled.
# Inerface Options > P1 Camera > Enable > Finish > Reboot

# To access MP4Box: sudo apt install -y gpac
# gpac 0.5.2-426
# Installs gpac gpac-modules-base libfreenect0.5 libglu1-mesa libgpac4



