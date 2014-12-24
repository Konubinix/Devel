#!/usr/bin/env python

import os
import gtk
import sys
import netrc

def responseToDialog(entry, dialog, response):
    dialog.response(response)

def getText(authInfo, authHost, authRealm):
    dialog = gtk.MessageDialog(
        None,
        gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
        gtk.MESSAGE_QUESTION,
        gtk.BUTTONS_OK_CANCEL,
        None)
    dialog.set_markup('%s at %s' % (authRealm, authHost))

    login = gtk.Entry()
    password = gtk.Entry()
    password.set_visibility(False)

    login.connect("activate", responseToDialog, dialog, gtk.RESPONSE_OK)
    password.connect("activate", responseToDialog, dialog, gtk.RESPONSE_OK)

    hbox = gtk.HBox();

    vbox_entries = gtk.VBox();
    vbox_labels = gtk.VBox();

    vbox_labels.pack_start(gtk.Label("Login:"), False, 5, 5)
    vbox_labels.pack_end(gtk.Label("Password:"), False, 5, 5)

    vbox_entries.pack_start(login)
    vbox_entries.pack_end(password)

    dialog.format_secondary_markup("Please enter username and password:")
    hbox.pack_start(vbox_labels, True, True, 0)
    hbox.pack_end(vbox_entries, True, True, 0)

    dialog.vbox.pack_start(hbox)
    dialog.show_all()
    rv = dialog.run()

    output = {
        'username': login.get_text(),
        'password': password.get_text()
    }
    dialog.destroy()
    return rv, output

def tryNetrc(authInfo, authHost, authRealm):
    net = netrc.netrc()
    authenticators = net.authenticators(authHost)
    rv = gtk.RESPONSE_NO
    output = {}

    if authenticators:
        (login, account, password) = authenticators
        rv = gtk.RESPONSE_OK
        output["username"] = login
        output["password"] = password
    return rv, output

if __name__ == '__main__':
    fifo = open(os.environ.get('UZBL_FIFO'), 'w')
    me, host, realm, retry, scheme, proxy, port, save = sys.argv
    rv, output = tryNetrc(me, host, realm)
    if (rv != gtk.RESPONSE_OK):
        rv, output = getText(me, host, realm)

    if (rv == gtk.RESPONSE_OK):
        print """AUTH
{}
{}""".format(output['username'], output['password'])
    else:
        exit(1)
