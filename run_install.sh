#!/bin/bash
##
 # Created by PyCharm
 # Author: tanismo
 # Project: Abissmal
 # Date: 01/25/2022
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
helper_path="${location}/Modules/helper.py"
cron_path="${location}/cron.sh"
sms_setup_path="${location}/Modules/Setup/twilioSMS.py"
bash_v=$(which bash)
python_v=$(which python)

echo -e "${Green}
           ____ _____  _____ _____ __  __          _
     /\   |  _ \_   _|/ ____/ ____|  \/  |   /\   | |
    /  \  | |_) || | | (___| (___ | \  / |  /  \  | |
   / /\ \ |  _ < | |  \___  \___ \| |\/| | / /\ \ | |
  / ____ \| |_) || |_ ____) |___) | |  | |/ ____ \| |____
 /_/    \_\____/_____|_____/_____/|_|  |_/_/    \_\______|

${NC}"
echo -e "${Green}Automated behavioral tracking by integrating sensors that survey movements around a target location${NC}"
echo -e "${Blue}Repository:${NC}  https://github.com/lastralab/abissmal"
echo -e "${Blue}Authors:${NC}     ${Cyan}Molina, T. & Smith-Vidaurre, G.${NC}"
echo ""
echo -e "${Yellow}Setting permissions for ${user_name}...${NC}"
find . -type f -exec chmod 644 {} \;
echo ""
echo -e "${Green}Enter the Box ID${NC} (Example: Box_01)"
echo -e "${Yellow}Press 'Enter' to skip configuration.${NC}"
read -r boxid
if [ -n "$boxid" ]
then
	sed -i "s/^box_id.*/box_id = '${boxid}'/" "${helper_path}"
  echo -e "${Purple}Registered ${boxid}${NC}"
else
	echo -e "${Yellow}Skipped.${NC}"
fi
sleep 1
echo ""

echo -e "${Green}Insert y/Y to install required packages or press 'Enter' to skip.${NC}"
read -r packs
if [ -n "$packs" ]
then
	echo -e "${Yellow}Installing packages:${NC}"
  apt-get update
  apt-get install fish
  apt-get install build-essential tk-dev libncurses5-dev libncursesw5-dev libreadline6-dev libdb5.3-dev libgdbm-dev libsqlite3-dev libssl-dev libbz2-dev libexpat1-dev liblzma-dev zlib1g-dev libffi-dev -y
  apt-get install python3
  curl -O https://bootstrap.pypa.io/get-pip.py
  python -m ensurepip --upgrade
  python get-pip.py
  python -m pip install --upgrade pip
  apt install python3-pip
  pip3 install wiringpi
  pip3 install rpi-gpio
  pip3 install picamera
  pip3 install twilio
  apt install -y vim
  apt-get install ntfs-3g
  apt-get install gparted
  apt-get install screen
  chmod +x Main.sh
  apt install nmap
  apt-get install -y gpac
  mv get-pip.py ../
  echo ""
else
	echo -e "${Yellow}Skipped.${NC}"
fi
echo ""

echo -e "${Green}Enter your Twilio Account SID to enable SMS alerts${NC}"
echo -e "${Yellow}Press 'Enter' to skip configuration.${NC}"
read -r sms
if [ -n "$sms" ]
then
  sed -i "s/^Sid.*/Sid = '${sms}'/" "${sms_setup_path}"
  echo -e "${Purple}Registered SID${NC}"
  echo ""
  echo -e "${Green}Enter your Twilio Account Token to enable SMS alerts${NC}"
  echo -e "${Yellow}Press 'Enter' to skip configuration.${NC}"
  read -r -s token
  if [ -n "$token" ]
  then
    sed -i "s/^Token.*/Token = '${token}'/" "${sms_setup_path}"
    echo -e "${Purple}Registered Token${NC}"
    echo ""
    echo -e "${Green}Enter Sender Phone Number.${NC}"
    echo -e "${Yellow}Press 'Enter' to skip configuration.${NC}"
    read -r sender
    if [ -n "$sender" ]
    then
      sed -i "s/^Sender.*/Sender = '${sender}'/" "${sms_setup_path}"
      echo -e "${Purple}Registered Sender${NC}"
      echo ""
      echo -e "${Green}Enter recipient number (s) to send alerts.${NC}"
      echo -e "${RED}Note:${NC} ${Yellow}Each number must be contained in single quotes ' ' and separated by a comma, as the example:${NC}"
      echo -e "${Cyan}'9998887766', '5554443322'${NC}"
      echo -e "${Yellow}Press 'Enter' to skip configuration.${NC}"
      echo ""
      read -r emails
      if [ -n "$emails" ]
      then
        sed -i "s/^Recipients.*/Recipients = [${emails}]/" "${sms_setup_path}"
        echo -e "${Purple}Registered recipient(s) = [${emails}]${NC}"
        sed -i "s/^Enabled.*/Enabled = True/" "${sms_setup_path}"
        echo ""
        echo -e "${BIGreen}Enabled Twilio SMS alerts${NC}"
        echo ""
      else
        echo -e "${Yellow}Skipped.${NC}"
        echo ""
      fi
    else
      echo -e "${Yellow}Skipped.${NC}"
      echo ""
    fi
  else
    echo -e "${Yellow}Skipped.${NC}"
    echo ""
  fi
else
	echo -e "${Yellow}Skipped.${NC}"
	echo ""
fi

sed -i "s#^location=.*#location=\"${location}\"#" "${cron_path}"
sed -i "s#^python_v=.*#python_v=\"${python_v}\"#" "${cron_path}"
chmod +x cron.sh
if grep -R "Abissmal" /etc/crontab
then
  echo -e "Abissmal Cron jobs already configured and will be logged in ${Cyan}/home/pi/log/abissmal_cron.log${NC}"
else
  sed -i -e "\$a0 0  * * *   pi ${bash_v} ${location}/cron.sh >> /home/pi/log/abissmal_cron.log" "/etc/crontab"
  echo -e "Abissmal Cron jobs will be logged in ${Cyan}/home/pi/log/abissmal_cron.log${NC}"
fi
service cron reload
echo -e "${Purple}Configured Cron Job to run every day at midnight${NC}"
echo ""
sleep 1
echo -e "${Green}Installation complete${NC}"
sleep 1
echo -e "${RED}Raspberry pi needs to be restarted at this point${NC}"
echo ""
echo -e "${RED}Restarting in 5 seconds...${NC}"
sleep 1
echo -e "${RED}4${NC}"
sleep 1
echo -e "${RED}3${NC}"
sleep 1
echo -e "${RED}2${NC}"
sleep 1
echo -e "${RED}1${NC}"
sleep 1
echo ""
reboot
