#!/usr/bin/env python
# -*- coding:utf-8 -*-

import inspect
import importlib

import logging
logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)

def import_module_n_helpers(module, ip, populate=True):
    try:
        ip.user_ns[module] = importlib.import_module(module)
        helpers_name = "ipython_helpers.helpers_{}".format(module)
        helpers = importlib.import_module(helpers_name)
        ip.user_ns[helpers_name] = helpers
        if populate:
            for name in dir(helpers):
                ip.user_ns[name] = getattr(helpers, name)
        logger.info("Imported {} and its helpers".format(module))
    except Exception as e:
        logger.exception("Could not import {} or modules".format(module))
