# ParentalCareTracking
**Authors:** <br>
Tania Molina<br>
Grace Smith-Vidaurre <br><br>

<h3>Overview</h3>
Temperature data is collected every minute all day and night. Parental visits are tracked using radio frequency identification (RFID) and infrared beam breakers all day and night. The RFID system requires an antenna at the entrance of the nest container and leg bands on birds with PIT tags. Two beam breakers are placed behind the RFID antenna to detect direction of movement (did a bird enter or leave?) and also to provide backup data in case the RFID antenna fails. Videos are recorded for short periods of time around parental visits during the day only.
This parental care tracking system was developed and implemented for captive zebra finches (*Taeniopygia guttata*). A main script controls data collection, error/warnings documentation in logs, data transfer to a USB, and e-mail alerts.

#Main Script
<p>Running all modules asynchronously and monitor results.</p>
<h3>Pre-requisites</h3>

- Screen: ```$ sudo apt-get install screen```
- SMTP setup to send email alerts from localhost
Purpose: Software to track parental care behavior in captive birds. This project requires a nest container and collects temperature data, parental visits, and video.
