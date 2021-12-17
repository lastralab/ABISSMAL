<h1>Parental Care Tracking</h1>
<b>Authors</b>: nmoltta, gsvidaurre <br>
<b>Date</b>: 22 November 2021

- Screen: ```$ sudo apt-get install screen```
- Fix Permissions: 
  - `````$ cd <path>/ParentalCareTracking/`````
  - ```$ chmod +x Main.sh```
- SMTP install/setup to send email alerts

<h3>Helper functions</h3>

- CSV writer
- Email Service
- Logging directory/files setup

<h1>Infrared beam breaker - IRBB</h1>

**Description**: This directory contains code to run and collect data from infrared beam breakers on a Raspberry Pi computer. Each beam breaker is a receiver - emitter pair. The code contained in IRBB.py was modified from from https://simonprickett.dev/using-a-break-beam-sensor-with-python-and-raspberry-pi/. The nest container design holds 2 beam breaker pairs. The lead beam breaker is the first that will be broken when a bird enters the tunnel into the nest chamber (e.g. the furthest beam breaker from the nest chamber). The rear beam breaker is the second that will be broken when a bird enters the tunnel into the nest chamber (e.g. the beam breaker nearest to the nest chamber).

**Data structure**: The function in IRBB.py should return the date and timestamp (HH:MM:SS) when an infrared beam is broken. The master script should return the recording chamber number, the beam breaker identity (lead or rear), the event label (e.g. "beam broken"), year, month, day, and timestamp (HH:MM:SS) of the event. Each of these fields will be written out as a new line of a .csv file generated per day by a separate function.

The master script sets up a pull up resistor to detect a falling edge that represents a beam break. Edges are detected by adding events rather than polling to avoid missing edges, and a bouncetime parameter ensures that only a single edge will be detected even when multiple callbacks occur in a rapid fashion.

Video is setup to start recording for 1 minute after the beam breaker gets activated. Date is logged and written in a csv file.

**Software**: To use the infrared beam breakers I ran the following code in a terminal window to install libraries:

sudo pip3 install signal

The RPi.GPIO, time, and datetime libraries were already installed.


<h1>Radio frequency identification - RFID</h1>

**Description**: This directory contains code to run and collect data from a 125kHz CognIot radio frequency identification (RFID) reader on a Raspberry Pi computer. The original code to control the RFID reader was obtained from https://github.com/CognIot/RFID_125kHz. The RFID reader is connected to an external circular antenna that sits at the entrance of the tunnel of the nest container (before both infrared beam breakers). The reader has been set to read EM4102 passive integrated transponder (PIT) tags held in a plastic leg bands made in sizes appropriate for different songbird species. This RFID system allows for identification of the individuals that enter and leave the nest chamber, and together with the infrared beam breakers, provides automated tracking of parental care behavior.

**Data structure**: The function in RFID.py should return the recording chamber number, the PIT tag identity (unique 16-bit string), year, month, day, and timestamp (HH:MM:SS). Each of these fields will be written out as a new line of a .csv file generated per day by a separate function.

<h1>Temperature sensor  - Temp</h1>

**Description**: This directory contains code to run and collect data from a waterproof 1-wire DS18B20 temperature sensor on a Raspberry Pi computer. The original code to control the temperature sensor was obtained from https://pimylifeup.com/raspberry-pi-temperature-sensor/. The sensor will provide temperature data from inside the nest chamber, but will not provide incubation temperature unless strategically placed to do so (which may be complicated by the fact that some birds continue to add nesting material and can bury the sensor). Temperature data inside the nest chamber complements parental visits captured by the RFID antenna and infrared beam breakers.

**Notes on usage**: The sensor must be connected to 3.3V for power and GPIO pin 4 for 1-wire data transfer. A single sub-folder per device should appear inside /sys/bus/w1/devices that starts with 28 after initial setup (see link above), ptherwise something is not connected correctly (e.g. when multiple folders appear that start with 00-). The script Temp.py must be executed with python3, otherwise some code will fail (e.g. the file = f statement in the print calls). May need to rerun setup of /w1 folder as in https://pimylifeup.com/raspberry-pi-temperature-sensor/ every time the Raspberry Pi computer restarts.

**Data structure**: The function in Temp.py should return the recording chamber number, the temperature value in degrees Celsius and degrees Farenheit, year, month, day, and timestamp (HH:MM:SS). Each of these fields will be written out as a new line of a .csv file generated per day by a separate function.


<h1>Video</h1>

**Description**: This directory contains code to run and collect videos from a Raspberry Pi (G) fisheye lens camera. Videos will be strategically recorded in h264 format to capture parent-offspring interactions within several seconds of visits to the nest chamber. These videos are intended to be used in later projects, and complement automated tracking of parental visits to the nest chamber currently obtained by the RFID antenna and infrared beam breakers.

**Notes on usage**: Since video data is computationally intensive to record, and expensive both to store and score, videos will be recorded around parental visits to the nest chamber. Either RFID or the infrared beam breakers should trigger video recording. Ideally, video recording will capture several seconds before the given parent enters (perhaps 10 seconds), and will record for a set amount of time after the trigger (e.g. 30 or 60 seconds). This code still needs to be optimized for resolution and size given the height of the nest container. A set of manually scored videos can possibly be used for automated tracking of behavior, but since these videos will be short it may be possible to score behaviors manually with an animal behavior app.

**Data structure**: The function in Video.py should return videos that contain the recording chamber number, and the year, month, day, and timestamp (HH:MM:SS) in the file name of each video.
