#!/bin/bash
# Terminal run:
# $ sudo apt-get install screen
# $ cd <<path to git directory>>/ParentalCareTracking/
# $ chmod +x Main.sh
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

echo ""
echo -e "${Blue}Project:${NC}     ${Green}PARENTAL CARE TRACKING ${NC}"
echo -e "${Blue}Repository:${NC}  https://github.com/lastralab/parentalcaretracking"
echo -e "${Blue}Date:${NC}        ${Green}November 2021${NC}"
echo -e "${Blue}Authors:${NC}     ${Green}Tania Molina & Grace Smith Vidaurre${NC}"
echo ""

echo -e "${Yellow}Setting permissions...${NC}"
find Modules/ -type f -exec chmod 644 {} \;
chown -R ${user_name} .
echo ""

echo -e "Starting screen name: ${Cyan}irbb${NC}..."
sleep 1s
set -x
screen -ls | grep Detached | cut -d. -f1 | awk '{print $1}' | xargs kill
screen -dm irbb
echo -e "Started ${Cyan}irbb${NC}."
screen -Sr irbb /usr/bin/python ${location}/Modules/IRBB.py
#screen -r irbb


# TODO: incorporate all modules to run on separate screens
# more info: https://raspi.tv/2012/using-screen-with-raspberry-pi-to-avoid-leaving-ssh-sessions-open
# screen bash -d -m -S irbb python Modules/IRBB.py

# TODO: include top command to track system performance
