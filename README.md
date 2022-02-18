<h1>ParentalCareTracking</h1>
<b>Authors:</b><br>
Tania Molina - <span style="pointer-events:none">tan@e-storecook.com</span><br>
Grace Smith-Vidaurre - <span style="pointer-events:none">github@smith-vidaurre.com</span>

Project board: https://github.com/lastralab/ParentalCareTracking/projects/1 <br>
Project wiki: https://github.com/lastralab/ParentalCareTracking/wiki

<h2>Project Overview</h2>
Software to track parental care behavior in captive birds. This project requires a nest container and collects temperature data, parental visits, and video.
<br><br>
Temperature data is collected every minute all day and night. Parental visits are tracked using radio frequency identification (RFID) and infrared beam breakers all day and night. The RFID system requires an antenna at the entrance of the nest container and leg bands on birds with PIT tags. Two beam breakers are placed behind the RFID antenna to detect direction of movement (did a bird enter or leave?) and also to provide backup data in case the RFID antenna fails. Videos are recorded for short periods of time around parental visits during the day only.
This parental care tracking system was developed and implemented for captive zebra finches (*Taeniopygia guttata*). A main script controls data collection, error/warnings documentation in logs, data transfer to a USB, and e-mail alerts.

# Installation

Please refer to our [Installation Overview](https://github.com/lastralab/ParentalCareTracking/wiki/Installation) before proceeding. There are a few pre-requisites to consider before running the installation script.

1. Run `sudo apt-get install git` in your RaspberryPi.
2. Go to desired directory to download this project.
3. Clone this repository `git clone https://github.com/lastralab/ParentalCareTracking.git`
4. From root directory `/path/to/ParentalCareTracking/` run:

   1. `sudo bash run_install.sh`
   2. Enter your password (we recommend to set a password for your raspberry pi)
   3. Insert information accordingly

### Troubleshooting

LED still ON after detaching all screens</h3>

- Open terminal in root folder, run:
  - > screen -r irbb
    >
  - Ctrl + C
