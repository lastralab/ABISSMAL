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
helper_path="${location}/Modules/helper.py"
email_setup_path="${location}/Modules/Setup/email_service.py"

echo ""
echo -e "${Blue}Project:${NC}     ${Green}P A R E N T A L   C A R E   T R A C K I N G${NC}"
echo -e "${Blue}Repository:${NC}  https://github.com/lastralab/parentalcaretracking"
echo -e "${Blue}Date:${NC}        ${Blue}November 2021${NC}"
echo -e "${Blue}Authors:${NC}     ${Cyan}Tania M. Molina & Grace Smith-Vidaurre${NC}"
echo ""
echo -e "${Yellow}Setting permissions for user:${NC} ${user_name}${NC}"
find ./ -type f -exec chmod 644 {} \;
chown -R "${user_name}" .
echo ""
echo -e "${BIGreen}Enter the Box ID${NC} (Example: Box_01)"
read -r boxid
sed -i "" "s/^box_id.*/box_id = '${boxid}'/" "${helper_path}"
echo -e "${Purple}Registered ${boxid}${NC}"
echo ""

echo -e "${BIGreen}Enter the email address to send emails from${NC} (gmail)"
read -r gmail
sed -i "" "s/^source.*/source = '${gmail}'/" "${email_setup_path}"
echo -e "${Purple}Registered ${gmail}${NC}"
echo ""

echo -e "${BIGreen}Enter the email password${NC}"
read -r -s pass
sed -i "" "s/^key.*/key = '${pass}'/" "${email_setup_path}"
echo -e "${Purple}Registered password${NC}"
echo ""

echo -e "${BIGreen}Enter email(s) to send error alerts.${NC}"
echo -e "Example: 'email1@gmail.com', 'email2@gmail.com'"
echo -e "${RED}Note: Each email must contain single quotes '' as the example${NC}"
read -r emails
sed -i "" "s/^emails.*/emails = [${emails}]/" "${helper_path}"
echo -e "${Purple}Registered emails = [${emails}]${NC}"
echo ""

# python_v=$(which python)
