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
    logging.FileHandler('/home/pi/log/' + name, mode='a', encoding=None, delay=False)
    logger = logging.getLogger('/home/pi/log/' + name)
    logger.setLevel(logging.DEBUG)
    logger.addHandler(handler)
    return logger
