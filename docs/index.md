# Welcome to ABISSMAL

Automated Behavioral tracking by Integrating Sensors that Survey Movements Around a target Location

For full documentation visit our [wiki](https://github.com/lastralab/ABISSMAL/wiki)

<pre class="dark">
           ____ _____  _____ _____ __  __          _      
     /\   |  _ \_   _|/ ____/ ____|  \/  |   /\   | |     
    /  \  | |_) || | | (___| (___ | \  / |  /  \  | |     
   / /\ \ |  _ < | |  \___ \\___ \| |\/| | / /\ \ | |     
  / ____ \| |_) || |_ ____) |___) | |  | |/ ____ \| |____ 
 /_/    \_\____/_____|_____/_____/|_|  |_/_/    \_\______|

</pre>

## Pre-requisites

* Setup Raspberry Pi by following [these steps](https://github.com/lastralab/Abissmal/wiki/2.-Set-up-Raspberry-Pi-and-tracking-system-software)
* Raspberry Pi 4 (2 or 4Gb)
* Raspberry Pi SD card (32Gb or 64 Gb with Rasbian Buster pre-installed)
* Python version: 3.7
* Setup [sensors and other hardware](https://github.com/lastralab/ABISSMAL/wiki/4.-Set-up-sensors-and-other-hardware)
- Before starting, get to know the basics of: [Linux Screen](https://linuxize.com/post/how-to-use-linux-screen/) and [Cron jobs](https://cronitor.io/guides/cron-jobs)

## Installing ABISSMAL

1. `cd ~/Desktop` - Navigate to the Raspberry Pi's Desktop directory
2. `git clone https://github.com/lastralab/ABISSMAL.git` - Clone the [repository](https://github.com/lastralab/ABISSMAL.git)
3. `cd ABISSMAL` - Access the project root directory
4. `sudo chmod +x *.sh` - Make files executable
5. `sudo ./run_install.sh` - Initiate the installation script, enter the following information: 
     * Insert `y/Y` to install required packages if it's the first time using ABISSMAL, this can be skipped later on. 
     * Enter your Twilio Account SID to enable SMS alerts or press "Enter" key to skip
     * Wait for it to restart
     * NOTE: If you are using SSH connection you will have to [mount the external hard drive manually](abissmal.md/#ssh-connection-external-drive-not-found)

## Directory Structure

    run_install.sh      # The installer script. This needs to run everytime you pull a new version of the repository
    Main.sh             # The script that initiates the ABISSMAL tracking system
    cron.sh             # This script is automatically used by ABISSMAL for data collection, system monitoring and error logging
    Modules/
        Backups.py      # Responsible for transfering data from the Pi to an external hard drive using cron
        monitor.py      # System monitoring
        IRBB.py         # Collects data from two pairs of infrared beam breakers
        RFID.py         # Collects data from the 125kHz CognIot radio frequency identification (RFID) reader
        Temp.py         # Collects data from the waterproof 1-wire DS18B20 temperature sensor
        Video.py        # Records videos by motion detection using a Raspberry Pi (H) infrared fisheye lens camera with infrared LEDs
        helper.py       # Contains helper functions such as the CSV writter, directory setup and sms alerts
        log.py          # Resposible for the global Logger setup
        alt_video.py    # Responsible to record alternative videos for validation (can only run on a raspberry pi without the other modules enabled)
    R/
        ...             # General documentation for data processing and integration functions
    3D/
        ...             # Holds 3D-printing designs for customized hardware in .stl format
    VideoConcatenation/
        ...             # Optional scripts for video concatenation with ffmpeg

