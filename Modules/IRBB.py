# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 12/8/21

import signal
import sys
import logging
import os
from datetime import date
from os.path import exists
from helper import logger_setup

home_pi = '/home/pi/'

# TODO change param to home_pi
logger_setup('/Users/nmoltta/Desktop/')

# test
print('IRBB ran...')
logging.info('IRBB running')
