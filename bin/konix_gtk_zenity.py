#!/usr/bin/env python2
import gtk
import sys
import konix_notify

import argparse
parser = argparse.ArgumentParser(description="""A poor man clone of zenity --entry.""")
parser.add_argument('-t','--text',
                    help="""The text to display""",
                    type=str,
                    required=True)
parser.add_argument('-i','--initial',
                    help="""The initial text""",
                    type=str)
parser.add_argument('--info',
                    help="""Do not ask any input""",
                    action="store_true")
parser.add_argument('-n','--no-clipboard',
                    help="""Disable clipboard handling""",
                    action="store_true")
args = parser.parse_args()

entry = None

def responseToDialog(entry, dialog, response):
    dialog.response(response)

def key_pressed(entry, event):
    if event.keyval == 65307: # echap
        sys.exit(1)

def get_from_clipboard(button=None, event=None):
    global entry
    clipboard_text = gtk.Clipboard(selection="PRIMARY").wait_for_text() or \
                     gtk.Clipboard(selection="CLIPBOARD").wait_for_text() or ""
    entry.set_text(clipboard_text)
    entry.grab_focus()
    konix_notify.main("Updated the entry with clipboard content")

def getText():
    #base this on a message dialog
    dialog = gtk.MessageDialog(
        None,
        gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
        gtk.MESSAGE_INFO if args.info else gtk.MESSAGE_QUESTION,
        gtk.BUTTONS_OK,
        None)
    dialog.set_markup(args.text)

    hbox = gtk.HBox()
    dialog.vbox.pack_end(hbox, True, True, 0)

    #create the text input field
    global entry
    entry = gtk.Entry()
    #allow the user to press enter to do ok
    entry.connect("activate", responseToDialog, dialog, gtk.RESPONSE_OK)
    entry.connect("key_press_event", key_pressed)
    #create a horizontal box to pack the entry and a label
    if args.initial:
        entry.set_text(args.initial)
    if not args.no_clipboard:
        get_from_clipboard()
        checkbutton = gtk.Button("Get clipboard content")
        checkbutton.connect("enter_notify_event", get_from_clipboard)

    if not args.info:
        hbox.pack_start(gtk.Label("Input:"), False, 5, 5)
        hbox.pack_end(entry)
        if not args.no_clipboard:
            dialog.vbox.pack_end(checkbutton)

    dialog.show_all()
    if not args.info:
        entry.grab_focus()
    dialog.run()
    text = entry.get_text()
    dialog.destroy()
    return text

if __name__ == '__main__':
    print getText(),
