#!/usr/bin/env python
# -*- coding:utf-8 -*-

import xmlrpclib
import os
server = xmlrpclib.ServerProxy(os.environ["KONIX_VOLUME_SERVER"])

from konix_android import droid

droid.dialogCreateSeekBar(
    int(server.get_volume()),
    150,
    "Volume",
    "Volume remote control")
droid.dialogShow()

while droid.eventWaitFor("dialog"):
    result = None
    while not result:
        result = droid.eventPoll().result
    if type(result) == list:
        result = result[0]
    progress = result["data"]["progress"]
    server.set_volume(str(progress) + "%")
