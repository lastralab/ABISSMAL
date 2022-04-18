<h1>Parental Care Tracking System</h1>
<b>Authors:</b><br>
Grace Smith-Vidaurre, PhD - <span style="pointer-events:none">grace&#64;smith<span style="display:none">&nbsp;</span>-vidaurre.com</span>
<br>
Tania Molina-Medrano - <span style="pointer-events:none;">tanis.mo&#64;e<span style="display:none">&nbsp;</span>-storecook.com</span><br>

Project wiki: https://github.com/lastralab/ParentalCareTracking/wiki

<h2>Project Overview</h2>
IoT system to track parental care behavior in captive birds. This project requires a nest container and collects temperature data, parental visits, and video.
<br><br>
Temperature data is collected every minute all day and night. Parental visits are tracked using radio frequency identification (RFID) and infrared beam breakers all day and night. The RFID system requires an antenna at the entrance of the nest container and leg bands on birds with PIT tags. Two beam breakers are placed behind the RFID antenna to detect direction of movement (did a bird enter or leave?) and also to provide backup data in case the RFID antenna fails. Videos are recorded for short periods of time around parental visits during the day only.
This parental care tracking system was developed and implemented for captive zebra finches (*Taeniopygia guttata*). A main script controls data collection, error/warnings documentation in logs, data transfer to a USB, and e-mail alerts.

# Pre-requisites
Please refer to our Wiki [Installation Overview](https://github.com/lastralab/ParentalCareTracking/wiki/Installation) before proceeding. There are a few pre-requisites to consider before running the installation script.


# Installing PCTS
<pre>
  _____   _____ _______ _____
 |  __ \ / ____|__   __/ ____|
 | |__) | |       | | | (___
 |  ___/| |       | |  \___ \
 | |    | |____   | |  ____) |
 |_|     \_____|  |_| |_____/
</pre>
1. Run `sudo apt-get install git` in your RaspberryPi.
2. Go to desired directory to download this project.
3. Clone this repository `git clone https://github.com/lastralab/ParentalCareTracking.git`
4. From root directory `/path/to/ParentalCareTracking/` run:

   1. `sudo bash run_install.sh`
   2. Enter your password (we recommend to set a password for your raspberry pi)
   3. Insert setup information accordingly
   4. Raspberry pi will be restarted automatically to apply changes
   5. If you are using ssh connection, you might need to <a href="https://github.com/lastralab/ParentalCareTracking/wiki/Installation#external-drive-not-found">mount the external drive</a> 
   6. Run `bash Main.sh` without sudo to start collecting data

## Troubleshooting

Please refer to our [Wiki](https://github.com/lastralab/ParentalCareTracking/wiki/Installation#troubleshooting)


