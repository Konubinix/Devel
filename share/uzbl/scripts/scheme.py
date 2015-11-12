#!/usr/bin/env python

import os
import subprocess
import sys
import konix_notify

try:
  import urllib.parse as urlparse
except ImportError:
  import urlparse


def detach_open(cmd, **kwargs):
    # Thanks to the vast knowledge of Laurence Withers (lwithers) and this message:
    # http://mail.python.org/pipermail/python-list/2006-November/587523.html
    if not os.fork():
        null = os.open(os.devnull, os.O_WRONLY)
        for i in range(3):
            os.dup2(null, i)
        os.close(null)
        subprocess.Popen(cmd, **kwargs)
    print('USED')


def main(uri):
    u = urlparse.urlparse(uri)

    if u.scheme == 'mailto':
        detach_open(['xterm', '-e', 'mail', u.path])
    elif u.scheme == 'xmpp':
        # Someone check for safe arguments to gajim-remote
        detach_open(['gajim-remote', 'open_chat', uri])
    elif u.scheme == 'git':
        detach_open(['git', 'clone', '--', uri], cwd=os.path.expanduser('~/src'))
    elif u.scheme == 'editor':
        dwim = subprocess.call(
            ['zenity', '--question', '--text', "Do what I mean ? "+uri]
        )
        if dwim == 0:
            detach_open(['konix_uri_dwim.py', uri],)
    elif u.scheme == 'smb' or u.scheme == "afp":
        smb_mount = os.environ.get("KONIX_SMB_MOUNT", None)
        if not smb_mount:
            konix_notify.main("KONIX_SMB_MOUNT not set")
            print("USED")
        else:
            real_path = os.path.join(smb_mount, u.netloc, u.path[1:])
            konix_notify.main("Opening %s with mimeopen" % real_path)
            detach_open(['mimeopen', "-n", real_path])

if __name__ == '__main__':
    try:
        uri = sys.argv[1]
    except IndexError:
        print('Error: No uri given to handle.')

        os.exit(1)

    main(uri)
