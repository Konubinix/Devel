#!/usr/bin/env python
# -*- coding:utf-8 -*-

import os

def get_folders():
    with_trash = bool(int(os.environ.get("KONIX_OFFLINEIMAP_FOLDERS_WITH_TRASH", "0")))
    # folders = ['[Gmail]/All Mail','[Gmail]/Tous les messages',]
    folders = ['INBOX', 'Drafts', 'Sent']
    if with_trash:
        folders += ['[Gmail]/Corbeille','[Gmail]/Bin','[Gmail]/Trash',]
    return folders
