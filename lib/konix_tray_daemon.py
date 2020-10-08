#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import gi
gi.require_version('Gtk', '3.0')
from gi.repository import Gtk as gtk
from gi.repository import GObject as gobject
from gi.repository import GdkPixbuf
import logging
logging.basicConfig(level=logging.DEBUG)
LOGGER = logging.getLogger(__name__)

import redis

db = redis.StrictRedis()

class TrayDaemon(object):
    def __init__(self, fifo_name, period, command_dict, default_command):
        """Make sure the fifo exists and is a fifo
        command_dict : must be a dict of the form
        { "command" : "icon file name" }
        default_command : the default icon to display
        """
        self.period = period
        self.command_dict = command_dict
        # replace the file names with the associated pixbuf
        for command in command_dict.keys():
            command_dict[command] = GdkPixbuf.Pixbuf.new_from_file(
                command_dict[command]
            )
        self.tray_icon = gtk.StatusIcon()
        self.tray_icon.set_from_pixbuf(command_dict[default_command])
        self.tray_icon.set_visible(True)
        self.fifo_name = fifo_name

    def update_tray(self):
        command = db.lpop(self.fifo_name)
        command = (command or "").strip()
        custom_icon = self.command_dict.get(command, None)
        if custom_icon:
            LOGGER.info("Custom command " + command)
            self.tray_icon.set_from_pixbuf(custom_icon)
        elif command == 'v':
            LOGGER.info("Becoming invisible")
            self.tray_icon.set_visible(False)
        elif command == 'V':
            LOGGER.info("Becoming visible")
            self.tray_icon.set_visible(True)
        elif command == 'Q':
            self.clean()
            LOGGER.info("Quitting")
            gtk.main_quit()
        return True

    def main(self):
        gobject.timeout_add(self.period, TrayDaemon.update_tray, self)
        gtk.main()

    def clean(self):
        """Clean the fifo"""
        os.unlink(self.fifo_file_name)

    def __del__(self):
        self.clean()
