<h1>ParentalCareTracking</h1>
<b>Authors:</b><br>
Tania Molina<br>
Grace Smith-Vidaurre

Project board: https://github.com/lastralab/ParentalCareTracking/projects/1 <br>
<h2>Git-flow</h2>
- All branches get created from <b>main</b><br>
- All pull requests get created to <b>staging</b><br>
- Only tested and validated code from staging gets merged to main<br>

<h2>Overview</h2>
Software to track parental care behavior in captive birds. This project requires a nest container and collects temperature data, parental visits, and video.
<br><br>
Temperature data is collected every minute all day and night. Parental visits are tracked using radio frequency identification (RFID) and infrared beam breakers all day and night. The RFID system requires an antenna at the entrance of the nest container and leg bands on birds with PIT tags. Two beam breakers are placed behind the RFID antenna to detect direction of movement (did a bird enter or leave?) and also to provide backup data in case the RFID antenna fails. Videos are recorded for short periods of time around parental visits during the day only.
This parental care tracking system was developed and implemented for captive zebra finches (*Taeniopygia guttata*). A main script controls data collection, error/warnings documentation in logs, data transfer to a USB, and e-mail alerts.
<h3>Pre-requisites</h3>

- Screen: ```$ sudo apt-get install screen```
  - Go to screen: ````$ screen -r name ````
  - Exit module: ````CTRL+C ````
- SMTP
  - TODO: setup to send email alerts from localhost

<h2>Main Script</h2>
<p>Runs all modules asynchronously and provides monitoring access via SSH.</p>

<h2>Remote Access Setup - SSH</h2>
- Change Raspberry Pi default password to avoid unwanted users
- Write all passwords in a safe place
- Follow steps from <a href="https://www.raspberrypi.com/documentation/computers/remote-access.html">Raspberry Pi Documentation - Remote Access</a>:
  - Find IP address
  - Enable SSH
- > sudo apt-get update
- > sudo apt install nmap
- > hostname -I
- > nmap -sn {hostname}/24
  - > Nmap scan report for {machine} (hostname)
- > ping {machine}
- From remote computer
  - > ssh pi@{hostname}

<h2>Hardware To Do</h2>
- Make RFID antenna to fit container entrance
- Solder proto-PCB after code testing
- Set up RPi, proto-PCB, and external hard drive in tupperwares


<h2>Troubleshooting</h2>
<h3>LED still ON after detaching all screens</h3>
- Open terminal in root folder, run:
  - > screen -r irbb
  - Ctrl + C
