#!/usr/bin/env python
# -*- coding:utf-8 -*-
import logging
import subprocess
import shlex
import xmlrpclib
import os
import sys
logging.basicConfig(level=logging.DEBUG)
LOGGER = logging.getLogger(__name__)

def by_konubinix_notificator(message, unique=False, duration=3000, type_="normal"):
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
    import pynotify
    if not pynotify.init("Message"):
        sys.exit(1)
    n = pynotify.Notification("Message", message)
    n.set_timeout(3000)
    n.show()

def by_sl4a(message, type_):
    from konix_android import droid
    import andlib
    if type_ == "normal":
        droid.makeToast(message)
    if type_ == "annoying":
        droid.notify("Konix Notify", message)
        droid.vibrate(1000)
    if type_ == "boring":
        droid.vibrate(1000)
        droid.notify("Konix Notify", message)
        droid.dialogDismiss()
        andlib.display(droid, "Konix notify", message, wait=False)

def by_pyosd(message):
    import pyosd
    p = pyosd.osd("-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-1", colour="green", pos=pyosd.POS_BOT,offset=40, align=pyosd.ALIGN_CENTER)
    p.display(message)

def send_to_phone(message, type_):
    try:
        p = subprocess.Popen(
            ["konix_context_url.sh", "-e", "adb", os.environ["PHONE_NAME"]],
            stdout=subprocess.PIPE
        )
        ip, _ = p.communicate()
        p.wait()
        if ip:
            server = xmlrpclib.ServerProxy("http://{}:9000/RPC2".format(ip))
            server.notify("{}: {}".format(os.environ.get("HOSTNAME", "unknown host"), message), type_)
    except:
        main("Phone not available", duration=500, to_phone=False)

def to_phone_subprocess(message, type_):
    subprocess.Popen(
        [
            "konix_display.py",
            "-t", str(type_),
            "-o",
            message
        ]
    )

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
        print(e.message)
        try:
            LOGGER.error("Fallbacking to pynotify")
            by_pynotify(message)
        except:
            LOGGER.error("Fallbacking to pyosd")
            try:
                by_pyosd(message)
            except:
                by_sl4a(message, type_=type_)
