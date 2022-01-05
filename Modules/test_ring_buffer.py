import io
import random
import picamera
from PIL import Image

# Code taken from picamera advanced recipes, image comparison from link below
# Check out "lightweight Python motion detection" https://forums.raspberrypi.com/viewtopic.php?t=45235&sid=b32ca9b34abf8243de9b15ddff22faf0

path = "/home/pi/Data_ParentalCareTracking/Video/"
prior_image = None

# Video resolution settings
video_width = 1280
video_height = 720

# Stream (ring buffer) and recording settings
stream_duration = 5
record_duration = 10

# Motion detection settings:
# Threshold (how much a pixel has to change by to be marked as "changed")
# Sensitivity (how many changed pixels before capturing an image)
threshold = 10
sensitivity = 20

def detect_motion(camera):
    global prior_image
    stream = io.BytesIO()
    camera.capture(stream, format='jpeg', use_video_port=True)
    stream.seek(0)
    if prior_image is None:
        prior_image = Image.open(stream)
        return False
    else:
        buffer1 = prior_image.load()
        
        current_image = Image.open(stream)
        buffer2 = current_image.load()
        # Compare current_image to prior_image to detect motion. This is
        # left as an exercise for the reader!
        #result = random.randint(0, 10) == 0
        
        # Count changed pixels
        changedPixels = 0
        # xrange no longer exists in Python 3
        for x in range(0, 100):
            for y in range(0, 75):
                # Check green channel only (originally noted to be the "highest quality" channel)
                pixdiff = abs(buffer1[x,y][1] - buffer2[x,y][1])
                if pixdiff > threshold:
                    changedPixels += 1
                    
                    # TKTK
                    
        # Set motion trigger if sufficient pixels changed
        if changedPixels > sensitivity:
            result = True
        else:
            result = False
        
        # Once motion detection is done, make the prior image the current
        prior_image = current_image
        return result

with picamera.PiCamera() as camera:
    camera.resolution = (video_width, video_height)
    stream = picamera.PiCameraCircularIO(camera, seconds = stream_duration)
    camera.start_recording(stream, format = 'h264')
    try:
        while True:
            camera.wait_recording(1)
            if detect_motion(camera):
                print('Motion detected; Recording started')
                # As soon as we detect motion, split the recording to
                # record the frames "after" motion
                camera.split_recording(path + 'after.h264')
                # Record for the specified duration of time after motion detection
                camera.wait_recording(record_duration)
                # Write the 10 seconds "before" motion to disk as well
                stream.copy_to(path + 'before.h264', seconds = stream_duration)
                stream.clear()
                print('Recording finished')
                
                # Wait briefly, then split recording back to the in-memory circular buffer
                camera.wait_recording(1)
                camera.split_recording(stream)
    finally:
        camera.stop_recording()