#!/usr/bin/env python3

from gi.repository import GObject as gobject
from gi.repository import GLib



import dbus
import dbus.service
import dbus.mainloop.glib
import notify2
import logging
logging.basicConfig(level=logging.DEBUG)


class NotificationException(dbus.DBusException):
        _dbus_error_name = 'konubinix.notificator.NotificationException'


class Notification(dbus.service.Object):
    notification = notify2.Notification("Message", "")
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
            notif = notify2.Notification("Message", message)
        notif.set_urgency(urgency)
        notif.show()

if __name__ == '__main__':
    notify2.init("Konubinix Notificator")

    dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)
    session_bus = dbus.SessionBus()

    name = dbus.service.BusName("konubinix.notificator", session_bus)
    notif = Notification(session_bus, '/Notification')

    mainloop = GLib.MainLoop()
    mainloop.run()
