#!/bin/bash
##
 # Created by PyCharm
 # Author: tanismo
 # Project: Abissmal
 # Date: 03/08/2022
##

user_name=$(whoami)
location=null
python_v=null

irbb_file="/Modules/IRBB.py"
irbb_command="${python_v} ${location}${irbb_file}"

temp_file="/Modules/Temp.py"
temp_command="${python_v} ${location}${temp_file}"

rfid_file="/Modules/RFID.py"
rfid_command="${python_v} ${location}${rfid_file}"

monitor_file="/Modules/monitor.py"
monitor_command="${python_v} ${location}${monitor_file}"

video_file="/Modules/Video.py"
video_command="${python_v} ${location}${video_file}"

modules=null

find Modules/ -type f -exec chmod 644 {} \;

if [[ "$modules" != null && "$location" != null ]];
then
  if [[ $modules == *"V"* || $modules == *"v"* ]];
  then
      screen -ls | grep video | cut -d. -f1 | awk '{print $1}' | xargs kill
      echo "$(date): Video screen killed."
      sleep 1s
      screen -dmS video bash -c "${video_command}"
      echo "$(date): Video screen restarted."
  fi
  if [[ $modules == *"T"* || $modules == *"t"* ]];
  then
      screen -ls | grep temp | cut -d. -f1 | awk '{print $1}' | xargs kill
      echo "$(date): Temp screen killed."
      sleep 1s
      screen -dmS temp bash -c "${temp_command}"
      echo "$(date): Temp screen restarted."
  fi
  if [[ $modules == *"R"* || $modules == *"r"* ]];
  then
      screen -ls | grep rfid | cut -d. -f1 | awk '{print $1}' | xargs kill
      echo "$(date): RFID screen killed."
      sleep 1s
      screen -dmS rfid bash -c "${rfid_command}"
      echo "$(date): RFID screen restarted."
  fi
  if [[ $modules == *"I"* || $modules == *"i"* ]];
  then
      screen -ls | grep irbb | cut -d. -f1 | awk '{print $1}' | xargs kill
      echo "$(date): IRBB screen killed."
      sleep 1s
      screen -dmS irbb bash -c "${irbb_command}"
      echo "$(date): IRBB screen restarted."
  fi
else
  echo "$(date) ERROR Cron not configured correctly. Run 'sudo bash run_install.sh' again if necessary."
fi
screen -ls | grep monitor | cut -d. -f1 | awk '{print $1}' | xargs kill
echo "$(date): Monitor screen killed."
sleep 1s
screen -dmS monitor bash -c "${monitor_command}"
echo "$(date): Monitor screen restarted."
sleep 1s
echo "$(date): Ran Abissmal cron job as ${user_name}"
