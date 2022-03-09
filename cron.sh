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

echo ""
echo -e "${Blue}Project:${NC}      ${Green}P A R E N T A L   C A R E   T R A C K I N G${NC}"
echo -e "${Blue}Repository:${NC}   ${Blue}https://github.com/lastralab/parentalcaretracking"${NC}
echo -e "${Blue}Last Updated:${NC} ${Blue}March 2022${NC}"
echo -e "${Blue}Script:${NC}       ${Blue}Cron Job${NC}"
echo -e "${Blue}Authors:${NC}      ${Cyan}Tania M. Molina${NC} & ${Cyan}Grace Smith-Vidaurre${NC}"
echo ""
