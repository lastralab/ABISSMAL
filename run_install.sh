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
echo -e "${BIGreen}Enter the Box ID${NC} (Example: Box_01):"
read -r boxid
sed -i "s/^(box_id = 'Box_01').*/(box_id = '${boxid}')/" "${helper_path}"
echo -e "${Purple}Registered ${boxid}${NC}"
echo ""

# python_v=$(which python)
