#!/usr/bin/env python
# -*- coding:utf-8 -*-

import os
import gtk
import gobject
import time
import logging
logging.basicConfig()
LOGGER = logging.getLogger(__name__)

class TrayDaemon(object):
    def init_fifo(self, fifo_file_name):
        self.fifo_file_name = fifo_file_name
        if os.path.exists(self.fifo_file_name):
            LOGGER.warning(self.fifo_file_name+" already exists, it should not")
            os.unlink(self.fifo_file_name)
        os.mkfifo(self.fifo_file_name)
        self.fifo = os.fdopen(os.open(self.fifo_file_name, os.O_RDONLY|os.O_NONBLOCK))

    def __init__(self, fifo_file_name, period, command_dict, default_command):
        """Make sure the fifo exists and is a fifo
        command_dict : must be a dict of the form { "command" : "icon file name" }
        default_command : the default icon to display
        """
        self.period = period
        # ####################################################################################################
        # Init the icons
        # ####################################################################################################
        self.command_dict = command_dict
        # replace the file names with the associated pixbuf
        for command in command_dict.keys():
            command_dict[command] = gtk.gdk.pixbuf_new_from_file(command_dict[command])
        self.tray_icon = gtk.StatusIcon()
        self.tray_icon.set_from_pixbuf(command_dict[default_command])
        self.tray_icon.set_visible(True)
        # ####################################################################################################

        self.init_fifo(fifo_file_name)

    def update_tray(self):
        command = self.fifo.read(1)
        custom_icon = self.command_dict.get(command, None)
        if custom_icon:
            LOGGER.info("Custom command "+command)
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
        self.fifo.close()
        os.unlink(self.fifo_file_name)

    def __del__(self):
        self.clean()
