# Created by PyCharm
# Author: nmoltta
# Project: ParentalCareTracking
# Date: 2/26/22

# !/usr/bin/env python3

import logging
from importlib import reload


def setup_custom_logger(name):
    logging.shutdown()
    reload(logging)
    formatter = logging.Formatter(fmt="%(asctime)s %(levelname)s %(message)s")
    handler = logging.StreamHandler()
    handler.setFormatter(formatter)
    logger = logging.getLogger(name)
    logger.handlers.clear()
    logger.setLevel(logging.DEBUG)
    logger.addHandler(handler)
    logging.FileHandler('/home/pi/log/' + name, mode='a', encoding=None, delay=False)
    logging.basicConfig(
        level=logging.DEBUG,
        filename='/home/pi/log/' + name,
        format='%(asctime)s [%(levelname)s] %(message)s',
        datefmt="%Y-%m-%d %H:%M:%S")
    return logger
