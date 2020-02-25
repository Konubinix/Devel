#!/usr/bin/env python
# -*- coding:utf-8 -*-
import logging
import subprocess
import shlex
from six.moves.xmlrpc_client import ServerProxy
import os
import sys
logging.basicConfig(level=logging.DEBUG)
LOGGER = logging.getLogger(__name__)

def by_konubinix_notificator(message, unique=False, duration=3000,
                             type_="normal"):
    import dbus
    session = dbus.SessionBus()
    path = "/Notification"
    LOGGER.info("Opening communication with "+path)
    notification = session.get_object('konubinix.notificator',
                                      path)
    LOGGER.info("Displaying message "+message)
    urgency = 1
    if type_ == "annoying":
        duration = 0
        unique = False
    elif type_ == "boring":
        duration = 0
        unique = False
        urgency = 2

    notification.Notify(message, duration, unique, urgency)
    LOGGER.info("Message displayed")

def by_pynotify(message):
    import notify2
    if not notify2.init("Message"):
        sys.exit(1)
    n = notify2.Notification("Message", message)
    n.set_timeout(3000)
    n.show()

def by_sl4a(message, type_):
    from konix_android import droid
    import andlib
    if type_ == "normal":
        droid.makeToast(message)
    if type_ == "annoying":
        droid.notify("!", message)
        droid.vibrate(1000)
    if type_ == "boring":
        droid.vibrate(1000)
        droid.notify("!", message)
        droid.dialogDismiss()
        andlib.display(droid, "!", message, wait=False)

def by_pyosd(message):
    import pyosd
    p = pyosd.osd("-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-1", colour="green", pos=pyosd.POS_BOT,offset=40, align=pyosd.ALIGN_CENTER)
    p.display(message)


def send_to_phone(message, type_):
    here = os.path.exists(os.path.expanduser("~/.here"))
    priority = {
        "normal": 0 if here else 2,
        "annoying": 4,
        "boring": 8,
    }[type_]
    os.system("clk gotify 'Notify' '{}' -p '{}'".format(message, priority))


# type_ is normal or annoying or boring
def main(message, unique=False, duration=3000, type_="normal", to_phone=False):
    message = message.replace("<", "-")
    try:
        local_display(message, unique, duration, type_)
    except:
        LOGGER.critical("Could not display locally")
    if to_phone:
        send_to_phone(message, type_)


def local_display(message, unique=False, duration=3000, type_="normal"):
    try:
        LOGGER.info("Trying with the notificator")
        by_konubinix_notificator(message, unique, duration, type_)
    except Exception as e:
        try:
            LOGGER.error("Fallbacking to notify2")
            by_pynotify(message)
        except:
            LOGGER.error("Fallbacking to pyosd")
            try:
                by_pyosd(message)
            except:
                by_sl4a(message, type_=type_)
