# Created by PyCharm
# Author: nmoltta
# Project: Abissmal
# Date: 2/26/22

# !/usr/bin/env python3

import logging
from importlib import reload
import os


def setup_custom_logger(name):
    if os.path.exists('/home/pi/log/' + name):
        logger = logging.getLogger('/home/pi/log/' + name)
        format_log = "%(asctime)s %(levelname)s %(message)s"
        logging.basicConfig(
            format=format_log,
            filename='/home/pi/log/' + name,
            level=logging.INFO,
            datefmt="%Y-%m-%d %H:%M:%S"
        )
    else:
        logging.shutdown()
        reload(logging)
        logging.FileHandler('/home/pi/log/' + name)
        logger = logging.getLogger('/home/pi/log/' + name)
        format_log = "%(asctime)s %(levelname)s %(message)s"
        logging.basicConfig(
            format=format_log,
            filename='/home/pi/log/' + name,
            level=logging.INFO,
            datefmt="%Y-%m-%d %H:%M:%S"
        )
    return logger
