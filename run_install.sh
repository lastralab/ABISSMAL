#!/bin/bash
##
 # Created by PyCharm
 # Author: nmoltta
 # Project: ParentalCareTracking
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
host_name=$(hostname)
helper_path="${location}/Modules/helper.py"
cron_path="${location}/cron.sh"
email_setup_path="${location}/Modules/Setup/email_service.py"
email_config_path="${location}/Modules/Setup/ssmtp.conf"
hostname_path="/etc/hostname"
hosts_path="/etc/hosts"
bash_v=$(which bash)
python_v=$(which python)

echo ""
echo -e "${Blue}Project:${NC}     ${Green}P A R E N T A L   C A R E   T R A C K I N G${NC}"
echo -e "${Blue}Repository:${NC}  https://github.com/lastralab/parentalcaretracking"
echo -e "${Blue}Installation script:${NC}        ${Blue}run_install.sh${NC}"
echo -e "${Blue}Authors:${NC}     ${Cyan}Tania M. Molina & Grace Smith-Vidaurre${NC}"
echo ""
echo -e "${Yellow}Setting permissions for ${user_name}...${NC}"
find . -type f -exec chmod 644 {} \;
echo ""
echo -e "${BIGreen}Enter the Box ID${NC} (Example: Box_01)"
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

echo -e "${Yellow}Insert any key to install required packages or press 'Enter' to skip.${NC}"
read -r packs
if [ -n "$packs" ]
then
	echo -e "${Yellow}Installing packages:${NC}"
  apt-get update
  apt-get install ntfs-3g
  apt-get install python3
  apt-get install gparted
  apt-get install screen
  chmod +x Main.sh
  apt-get install ssmtp
  apt install nmap
  echo ""
else
	echo -e "${Yellow}Skipped.${NC}"
fi
echo ""

echo -e "${BIGreen}Enter the email address to send emails from${NC} (gmail)"
echo -e "${Yellow}Press 'Enter' to skip configuration.${NC}"
read -r gmail
if [ -n "$gmail" ]
then
	sed -i "s/^source.*/source = '${gmail}'/" "${email_setup_path}"
  sed -i "s/^AuthUser.*/AuthUser=${gmail}/" "${email_config_path}"
  echo -e "${Purple}Registered ${gmail}${NC}"
else
	echo -e "${Yellow}Skipped.${NC}"
fi
echo ""

echo -e "${BIGreen}Enter the email password (Note: Do not copy & paste)${NC}"
echo -e "${Yellow}Press 'Enter' to skip configuration.${NC}"
read -r -s pass
if [ -n "$pass" ]
then
	sed -i "s/^key.*/key = '${pass}'/" "${email_setup_path}"
  sed -i "s/^AuthPass.*/AuthPass=${pass}/" "${email_config_path}"
  echo -e "${Purple}Registered password${NC}"
else
	echo -e "${Yellow}Skipped.${NC}"
fi
echo ""

echo -e "${BIGreen}Enter email(s) to send error alerts.${NC}"
echo -e "${RED}Note:${NC} ${Yellow}Each email must be contained in single quotes ' ' as the example:${NC}"
echo -e "${Cyan}'email1@gmail.com', 'email2@gmail.com'${NC}"
echo ""
echo -e "${Yellow}Press 'Enter' to skip configuration.${NC}"
read -r emails
if [ -n "$emails" ]
then
	sed -i "s/^emails.*/emails = [${emails}]/" "${helper_path}"
  echo -e "${Purple}Registered emails = [${emails}]${NC}"
else
	echo -e "${Yellow}Skipped.${NC}"
fi
echo ""

echo -e "${Yellow}Insert 'Y/y' to configure Cron or press 'Enter' to skip.${NC}"
read -r cron
if [ -n "$cron" ]
then
  sed -i -e "\$a0 0  * * *   pi ${bash_v} ${location}/cron.sh" "/etc/crontab"
  sed -i "s/^location.*/location=\"${location}\"/" "${cron_path}"
  sed -i "s/^python_v.*/python_v=\"${python_v}\"/" "${cron_path}"
  chmod +x cron.sh
  service cron reload
  echo -e "${Purple}Configured Cron Job to run every 6 hrs at the 30th minute${NC}"
else
	echo -e "${Yellow}Skipped.${NC}"
fi
echo ""

echo -e "${BIGreen}Enter the new hostname to configure email service${NC} (Example: raspberrypi + box number)"
echo -e "${Yellow}Press 'Enter' to skip configuration.${NC}"
read -r hostname
if [ -n "$hostname" ]
then
	sed -i "s/^${host_name}.*/${hostname}/" "${hostname_path}"
  sed -i "s/^hostname.*/hostname=${hostname}/" "${email_config_path}"
  add="\$a127.0.1.1  ${hostname}"
  sed -i -e "${add}" "${hosts_path}"
  mv /etc/ssmtp/ssmtp.conf /etc/ssmtp/ssmtp.conf.sample
  cp -r "${email_config_path}" /etc/ssmtp/
  sleep 1
  chmod +x cron.sh
  echo -e "${Purple}Registered new hostname and updated hosts file${NC}"
  echo -e "${Green}Installation complete.${NC}"
  sleep 1
  echo -e "${RED}Raspberry pi needs to be restarted at this point${NC}"
  echo ""
  echo -e "${RED}Restarting in 5 seconds...${NC}"
  sleep 6
  echo ""
  reboot
else
	echo -e "${Yellow}Skipped.${NC}"
fi
echo ""
echo -e "${Green}Installation complete.${NC}"
echo ""
