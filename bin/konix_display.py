#!/usr/bin/python
# -*- coding:utf-8 -*-
import sys
import logging
import konix_notify
logging.basicConfig(level=logging.DEBUG)

if len(sys.argv) <= 1:
    exit(1)
message = ' '.join(sys.argv[1:])
konix_notify.main(message)
