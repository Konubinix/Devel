#!/usr/bin/env python
# -*- coding:utf-8 -*-

def by_konubinix_notificator(message,unique=False):
    import dbus
    session = dbus.SessionBus()
    if unique:
        path = "/UniqueNotification"
    else:
        path = "/Notification"
    logging.info("Opening communication with "+path)
    notification = session.get_object('konubinix.notificator',
                                      path)
    logging.info("Displaying message "+message)
    notification.Notify(message)

def by_pynotify(message):
    import pynotify
    if not pynotify.init("Message"):
        sys.exit(1)
    n = pynotify.Notification("Message", message)
    n.set_timeout(3000)
    n.show()

def by_pyosd(message):
    import pyosd
    p = pyosd.osd("-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-1", colour="green", pos=pyosd.POS_BOT,offset=40, align=pyosd.ALIGN_CENTER)
    p.display(message)

def main(message,unique=False):
    try:
        by_konubinix_notificator(message,unique)
    except:
        try:
            by_pynotify(message)
        except:
            by_pyosd(message)
