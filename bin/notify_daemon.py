#!/usr/bin/env python

import gobject

import dbus
import dbus.service
import dbus.mainloop.glib
import pynotify

class NotificationException(dbus.DBusException):
	_dbus_error_name = 'konubinix.notificator.NotificationException'

class Notification(dbus.service.Object):
	notification = pynotify.Notification("Message", "")
	notification.set_timeout(3000);

	@dbus.service.method("konubinix.notificator",
						 in_signature='s', out_signature='')
	def Notify(self, message):
		notification = pynotify.Notification("Message", message)
		notification.set_timeout(3000);
		notification.show()

class UniqueNotification(Notification):
	notification = pynotify.Notification("Message", "")
	notification.set_timeout(3000);

	@dbus.service.method("konubinix.notificator",
						 in_signature='s', out_signature='')
	def Notify(self, message):
		self.notification.update("Message", message)
		self.notification.show()

if __name__ == '__main__':
	pynotify.init("Konubinix Notificator")
	dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)

	session_bus = dbus.SessionBus()
	name = dbus.service.BusName("konubinix.notificator", session_bus)
	Notification(session_bus, '/Notification')
	Notification.notification.set_timeout(3000)
	UniqueNotification(session_bus, '/UniqueNotification')

	mainloop = gobject.MainLoop()
	print "Running Konubinix Notificator."
	mainloop.run()
