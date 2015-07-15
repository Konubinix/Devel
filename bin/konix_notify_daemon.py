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
	notification.set_timeout(3000);

	@dbus.service.method("konubinix.notificator",
						 in_signature='si', out_signature='')
	def Notify(self, message, show_time):
                logging.info("Notifying message %s for time %s" % (message, show_time))
		notification = pynotify.Notification("Message", message)
                logging.debug("Notification element : "+str(notification))
		notification.set_timeout(show_time);
		print notification.show()

class UniqueNotification(Notification):
	notification = pynotify.Notification("Message", "")
	notification.set_timeout(3000);

	@dbus.service.method("konubinix.notificator",
						 in_signature='si', out_signature='')
	def Notify(self, message, show_time):
                logging.info("Notifying unique message "+message)
		self.notification.set_timeout(show_time);
		self.notification.update("Message", message)
		self.notification.show()

if __name__ == '__main__':
	pynotify.init("Konubinix Notificator")
	dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)

	session_bus = dbus.SessionBus()
	name = dbus.service.BusName("konubinix.notificator", session_bus)
        logging.debug("Registering on debus service "+str(name))
	Notification(session_bus, '/Notification')
	Notification.notification.set_timeout(3000)
	UniqueNotification(session_bus, '/UniqueNotification')

	mainloop = gobject.MainLoop()
	print "Running Konubinix Notificator."
	mainloop.run()
