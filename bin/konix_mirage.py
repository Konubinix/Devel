#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import mirage

import pygtk
pygtk.require('2.0')
import gtk

def mousewheel_scrolled(self, widget, event):
    if event.type == gtk.gdk.SCROLL:
        # Zooming of the image by mousewheel
        if not (event.state & gtk.gdk.CONTROL_MASK):
            if event.direction == gtk.gdk.SCROLL_UP:
                self.zoom_in(None)
            elif event.direction == gtk.gdk.SCROLL_DOWN:
                self.zoom_out(None)
            return True
        # Navigation of images with mousewheel:
        else:
            if event.direction == gtk.gdk.SCROLL_UP:
                self.goto_prev_image(None)
            elif event.direction == gtk.gdk.SCROLL_DOWN:
                self.goto_next_image(None)
            return True

mirage.Base.mousewheel_scrolled = mousewheel_scrolled

if __name__ == "__main__":
    app = mirage.Base()
    try:
        app.main()
    except KeyboardInterrupt:
        pass
