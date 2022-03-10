#!/bin/bash
##
 # Created by PyCharm
 # Author: nmoltta
 # Project: ParentalCareTracking
 # Date: 03/08/2022
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

temp_file="/Modules/Temp.py"
temp_command="${python_v} ${location}${temp_file}"

rfid_file="/Modules/RFID.py"
rfid_command="${python_v} ${location}${rfid_file}"

backups_file="/Modules/Backups.py"
backups_command="${python_v} ${location}${backups_file}"

monitor_file="/Modules/monitor.py"
monitor_command="${python_v} ${location}${monitor_file}"

echo ""
echo -e "${Blue}Project:${NC}      ${Green}P A R E N T A L   C A R E   T R A C K I N G${NC}"
echo -e "${Blue}Repository:${NC}   ${Blue}https://github.com/lastralab/parentalcaretracking"${NC}
echo -e "${Blue}Last Updated:${NC} ${Blue}March 2022${NC}"
echo -e "${Blue}Script:${NC}       ${Blue}Cron Job${NC}"
echo -e "${Blue}Authors:${NC}      ${Cyan}Tania M. Molina${NC} & ${Cyan}Grace Smith-Vidaurre${NC}"
echo ""

screen -ls | grep temp | cut -d. -f1 | awk '{print $1}' | xargs kill
screen -ls | grep irbb | cut -d. -f1 | awk '{print $1}' | xargs kill
screen -ls | grep rfid | cut -d. -f1 | awk '{print $1}' | xargs kill
screen -ls | grep backup | cut -d. -f1 | awk '{print $1}' | xargs kill
screen -ls | grep monitor | cut -d. -f1 | awk '{print $1}' | xargs kill
sleep 1s
screen -dmS temp bash -c "${temp_command}"
screen -dmS irbb bash -c "${irbb_command}"
screen -dmS rfid bash -c "${rfid_command}"
screen -dmS backup bash -c "${backups_command}"
screen -dmS monitor bash -c "${monitor_command}"