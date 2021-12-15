#!/bin/bash
# GSV 03 May 2021

# Notes: changed python to python3

if [ -f /home/pi/rfid_logs/$(date +"%m-%d-%y").txt ]; then
     stdbuf -oL python3 /home/pi/rfid_reader.py >> /home/pi/rfid_logs/$(date +"%m-%d-%y").txt &
else
     logger_id=site_name
     lat=12.345678
     lon=-12.345678
     echo "$logger_id" > /home/pi/rfid_logs/$(date +"%m-%d-%y").txt
     echo "$lat, $lon" >> /home/pi/rfid_logs/$(date +"%m-%d-%y").txt
     stdbuf -oL python3 /home/pi/rfid_reader.py >> /home/pi/rfid_logs/$(date +"%m-%d-%y").txt &
fi
