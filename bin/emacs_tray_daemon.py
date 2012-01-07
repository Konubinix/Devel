#!/usr/bin/env python
# -*- coding:utf-8 -*-

import os
import gtk
import gobject
import tempfile
import time
import logging
logging.basicConfig()
LOGGER = logging.getLogger(__name__)

EMACS_TRAY_PERIOD = 200

class EmacsTrayDaemon(object):
    def init_fifo(self, fifo_file_name):
        self.fifo_file_name = fifo_file_name
        if os.path.exists(self.fifo_file_name):
            LOGGER.warning(self.fifo_file_name+" already exists, it should not")
            os.unlink(self.fifo_file_name)
        os.mkfifo(self.fifo_file_name)
        self.fifo = os.fdopen(os.open(self.fifo_file_name, os.O_RDONLY|os.O_NONBLOCK))

    def init_icons(self, icons_dir):
        self.blue_icon_file_name = os.path.join(icons_dir, "emacs_icon_blue.png")
        self.red_icon_file_name = os.path.join(icons_dir, "emacs_icon_red.png")

        self.blue_pixbuf = gtk.gdk.pixbuf_new_from_file(self.blue_icon_file_name)
        self.red_pixbuf = gtk.gdk.pixbuf_new_from_file(self.red_icon_file_name)

    def __init__(self, fifo_file_name):
        """Make sure the fifo exists and is a fifo"""
        # ####################################################################################################
        # Init the icons
        # ####################################################################################################
        share_dir = os.environ.get("KONIX_SHARE_DIR",
                                   os.path.join(os.environ["HOME"],"share"))
        icons_dir = os.path.join(share_dir, "icons")
        self.init_icons(icons_dir)

        self.tray_icon = gtk.StatusIcon()
        self.tray_icon.set_from_pixbuf(self.blue_pixbuf)
        self.tray_icon.set_visible(True)
        # ####################################################################################################

        self.init_fifo(fifo_file_name)

    def update_tray(self):
        command = self.fifo.read(1)
        if command == 'b':
            LOGGER.info("No more notification")
            self.tray_icon.set_from_pixbuf(self.blue_pixbuf)
        elif command == 'B':
            LOGGER.info("Notification coming")
            self.tray_icon.set_from_pixbuf(self.red_pixbuf)
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
        gobject.timeout_add(EMACS_TRAY_PERIOD, EmacsTrayDaemon.update_tray, self)
        gtk.main()

    def clean(self):
        """Clean the fifo"""
        self.fifo.close()
        os.unlink(self.fifo_file_name)

    def __del__(self):
        self.clean()

if __name__ == "__main__":
    etd = EmacsTrayDaemon(os.path.join(tempfile.gettempdir(), "tray_daemon_control"))
    etd.main()
