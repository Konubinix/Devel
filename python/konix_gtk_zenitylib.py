#!/usr/bin/env python3
import argparse
import sys

import konix_notify
from gi.repository import Gdk, GLib
from gi.repository import Gtk as gtk

parser = argparse.ArgumentParser(
    description="""A poor man clone of zenity --entry.""")
parser.add_argument('-t',
                    '--text',
                    help="""The text to display""",
                    type=str,
                    required=True)
parser.add_argument('-T',
                    '--timeout',
                    help="""Close after that amount of ms""",
                    type=int,
                    default=0,
                    required=False)
parser.add_argument('-i', '--initial', help="""The initial text""", type=str)
parser.add_argument('--info',
                    help="""Do not ask any input""",
                    action="store_true")
parser.add_argument('-n',
                    '--no-clipboard',
                    help="""Disable clipboard handling""",
                    action="store_true")
parser.add_argument('-a',
                    '--above-all',
                    help="""Set the window above all""",
                    action="store_true")
parser.add_argument('-b',
                    '--background-color',
                    help="""Can be anything that goes into
                    http://www.pygtk.org/pygtk2reference/class-gdkcolor.html#function-gdk--color-parse""",
                    type=str)

entry = None


def responseToDialog(entry, dialog, response):
    dialog.response(response)


def key_pressed(entry, event):
    if event.keyval == 65307:  # echap
        sys.exit(1)


def get_from_clipboard(button=None, event=None):
    global entry
    clipboard_text = gtk.Clipboard.get(Gdk.SELECTION_PRIMARY).wait_for_text() or \
                     gtk.Clipboard.get(Gdk.SELECTION_CLIPBOARD).wait_for_text() or ""
    entry.set_text(clipboard_text)
    entry.grab_focus()
    konix_notify.main("Updated the entry with clipboard content")


def getText(info=False,
            text="Something",
            initial="",
            clipboard=True,
            above_all=False,
            timeout=0,
            background_color=None):
    #base this on a message dialog
    text = text.replace("<", "_").replace(">", "_")
    dialog = gtk.MessageDialog(
        None, gtk.DialogFlags.MODAL | gtk.DialogFlags.DESTROY_WITH_PARENT,
        gtk.MessageType.INFO if info else gtk.MessageType.QUESTION,
        gtk.ButtonsType.NONE if info else gtk.ButtonsType.OK, None)
    dialog.set_title("konix_gtk_entry")
    dialog.set_markup(text)
    if background_color:
        css = '* { background-color: #f00; }'
        css_provider = gtk.CssProvider()
        css_provider.load_from_data(css.encode())
        context = gtk.StyleContext()
        screen = Gdk.Screen.get_default()
        context.add_provider_for_screen(
            screen, css_provider, gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)
    if above_all:
        dialog.set_keep_above(True)

    hbox = gtk.HBox()
    dialog.vbox.pack_end(hbox, True, True, 0)

    #create the text input field
    global entry
    entry = gtk.Entry()
    #allow the user to press enter to do ok
    entry.connect("activate", responseToDialog, dialog, gtk.ResponseType.OK)
    entry.connect("key_press_event", key_pressed)
    #create a horizontal box to pack the entry and a label
    if initial:
        entry.set_text(initial)
    if clipboard:
        get_from_clipboard()
        checkbutton = gtk.Button("Get clipboard content")
        checkbutton.connect("enter_notify_event", get_from_clipboard)

    if not info:
        hbox.pack_start(gtk.Label("Input:"), False, 5, 5)
        hbox.pack_end(entry, True, True, 0)
        if clipboard:
            dialog.vbox.pack_end(checkbutton, True, True, 0)

    dialog.show_all()

    if not info:
        entry.grab_focus()

    if timeout:

        def _quit():
            dialog.destroy()

        GLib.timeout_add(timeout, _quit)
    dialog.run()
    text = entry.get_text()
    dialog.destroy()
    return text


def main():
    args = parser.parse_args()
    print(
        getText(info=args.info,
                text=args.text,
                initial=args.initial,
                clipboard=not args.no_clipboard,
                above_all=args.above_all,
                timeout=args.timeout,
                background_color=args.background_color))


if __name__ == '__main__':
    main()
