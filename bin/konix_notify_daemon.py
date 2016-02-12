#!/usr/bin/env python2

import gobject

import dbus
import dbus.service
import dbus.mainloop.glib
import pynotify
import logging
logging.basicConfig(level=logging.DEBUG)

class NotificationException(dbus.DBusException):
        _dbus_error_name = 'konubinix.notificator.NotificationException'

class Notification(dbus.service.Object):
    notification = pynotify.Notification("Message", "")
    @dbus.service.method("konubinix.notificator",
                         in_signature='sibi', out_signature='')
    def Notify(self, message, show_time, unique, urgency):
        logging.info(
            "Notifying message {}{} for time {} of urgency {}".format(
                "unique " if unique else "",
                message,
                show_time,
                urgency,
            )
        )
        if unique:
            notif = self.notification
            notif.update("Message", message)
        else:
            notif = pynotify.Notification("Message", message)
        notif.set_urgency(urgency)
        notif.show()

if __name__ == '__main__':
    pynotify.init("Konubinix Notificator")
    dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)

    session_bus = dbus.SessionBus()
    name = dbus.service.BusName("konubinix.notificator", session_bus)
    logging.debug("Registering on debus service "+str(name))
    notif = Notification(session_bus, '/Notification')

    mainloop = gobject.MainLoop()
    print "Running Konubinix Notificator."
    mainloop.run()
