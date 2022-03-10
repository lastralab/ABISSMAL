#!/bin/bash
##
 # Created by PyCharm
 # Author: nmoltta
 # Project: ParentalCareTracking
 # Date: 12/08/2021
##

RED='\033[0;31m'
NC='\033[0m'
BIGreen='\033[1;92m'
Green='\033[0;92m'
Yellow='\033[0;93m'
Blue='\033[0;94m'
Purple='\033[0;95m'
Cyan='\033[0;96m'

user_name=$(whoami)
location=$(pwd)
python_v=$(which python)

irbb_file="/Modules/IRBB.py"
irbb_command="${python_v} ${location}${irbb_file}"

video_file="/Modules/Video.py"
video_command="${python_v} ${location}${video_file}"

temp_file="/Modules/Temp.py"
temp_command="${python_v} ${location}${temp_file}"

rfid_file="/Modules/RFID.py"
rfid_command="${python_v} ${location}${rfid_file}"

backups_file="/Modules/Backups.py"
backups_command="${python_v} ${location}${backups_file}"

monitor_file="/Modules/monitor.py"
monitor_command="${python_v} ${location}${monitor_file}"

echo ""
echo -e "${Blue}Project:${NC}     ${Green}P A R E N T A L   C A R E   T R A C K I N G${NC}"
echo -e "${Blue}Repository:${NC}  ${Blue}https://github.com/lastralab/parentalcaretracking"${NC}
echo -e "${Blue}Date:${NC}        ${Blue}November 2021${NC}"
echo -e "${Blue}Authors:${NC}     ${Cyan}Tania M. Molina${NC} & ${Cyan}Grace Smith-Vidaurre${NC}"
echo ""

echo -e "${Yellow}Insert y/Y to configure Cron or press 'Enter' to skip.${NC}"
read -r cron
if [ -n "$cron" ]
then
  chmod +x cron.sh
  (crontab -l 2>/dev/null; echo "0 0 * * * bash ${location}/cron.sh") | crontab -
  echo -e "${Purple}Configured Cron Job to run every day at midnight${NC}"
else
	echo -e "${Yellow}Skipped.${NC}"
fi
echo ""

echo -e "${Yellow}Setting permissions for user:${NC} ${user_name}"
echo ""
find Modules/ -type f -exec chmod 644 {} \;
chown -R "${user_name}" .
echo -e "To access a screen run:${Green} screen -r ${NC}${Purple}{name}${NC}"
echo -e "To detach a screen press${Blue} Ctrl + A${NC} then type ${Blue}:${NC} to enter command mode and use command ${RED}\"detach\"${NC}"
echo ""

echo -e "Starting screen name: ${Cyan}irbb${NC}..."
sleep 1s
screen -dmS irbb bash -c "${irbb_command}"

echo -e "Starting screen name: ${Cyan}video${NC}..."
sleep 1s
screen -dmS video bash -c "${video_command}"

echo -e "Starting screen name: ${Cyan}rfid${NC}..."
sleep 1s
screen -dmS rfid bash -c "${rfid_command}"

echo -e "Starting screen name: ${Cyan}temp${NC}..."
sleep 1s
screen -dmS temp bash -c "${temp_command}"

echo -e "Starting screen name: ${Cyan}backup${NC}..."
sleep 1s
screen -dmS backup bash -c "${backups_command}"

echo -e "Starting screen name: ${Cyan}monitor${NC}..."
sleep 1s
screen -dmS monitor bash -c "${monitor_command}"
echo ""
sleep 3s
screen -list

echo ""
echo -e "To kill all detached screens, run:"
echo -e "${BIGreen}screen -ls | grep Detached | cut -d. -f1 | awk '{print \$1}' | xargs kill${NC}"

echo ""

