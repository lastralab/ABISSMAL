<h1>ParentalCareTracking</h1>
<b>Authors:</b><br>
Tania Molina<br>
Grace Smith-Vidaurre

Project board: https://github.com/lastralab/ParentalCareTracking/projects/1 <br>
Project wiki: https://github.com/lastralab/ParentalCareTracking/wiki

<h2>Overview</h2>
Software to track parental care behavior in captive birds. This project requires a nest container and collects temperature data, parental visits, and video.
<br><br>
Temperature data is collected every minute all day and night. Parental visits are tracked using radio frequency identification (RFID) and infrared beam breakers all day and night. The RFID system requires an antenna at the entrance of the nest container and leg bands on birds with PIT tags. Two beam breakers are placed behind the RFID antenna to detect direction of movement (did a bird enter or leave?) and also to provide backup data in case the RFID antenna fails. Videos are recorded for short periods of time around parental visits during the day only.
This parental care tracking system was developed and implemented for captive zebra finches (*Taeniopygia guttata*). A main script controls data collection, error/warnings documentation in logs, data transfer to a USB, and e-mail alerts.

<h2>Main Script</h2>
<p>Runs all modules asynchronously and provides monitoring screens for each module to access via SSH.</p>

Remote Access Setup - SSH</h2>

- Change Raspberry Pi default password to avoid unwanted users
- Write all passwords in a safe place
- Follow steps from <a href="https://www.raspberrypi.com/documentation/computers/remote-access.html">Raspberry Pi Documentation - Remote Access</a>:
  - Find IP address
  - Enable SSH
- > sudo apt-get update
  >
- > sudo apt install nmap
  >
- > hostname -I
  >
- > nmap -sn {hostname}/24
  >

  - > Nmap scan report for {machine} (hostname)
    >
- > ping {machine}
  >
- From remote computer
  - > ssh pi@{hostname}
    >

Troubleshooting</h2>

LED still ON after detaching all screens</h3>

- Open terminal in root folder, run:
  - > screen -r irbb
    >
  - Ctrl + C
