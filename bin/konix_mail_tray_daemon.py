#!/usr/bin/env python2
# -*- coding:utf-8 -*-

import os
import tempfile

from konix_tray_daemon import TrayDaemon

def main():
    """
    """
    share_dir = os.environ.get("KONIX_SHARE_DIR",
                               os.path.join(os.environ["HOME"],"share"))
    icons_dir = os.path.join(share_dir, "icons")

    mtd = TrayDaemon(os.path.join(tempfile.gettempdir(),
                                  "mail_tray_daemon_control"),
                     200,
                     {"i" : os.path.join(icons_dir, "mail_icon_empty.png"),
                      "n" : os.path.join(icons_dir, "mail_icon_new_mail.png"),
                      "N" : os.path.join(icons_dir, "mail_icon_new_mail_unread.png"),
                      "u" : os.path.join(icons_dir, "mail_icon_unread.png"),
                      "f" : os.path.join(icons_dir, "mail_icon_flagged.png"),
                      "F" : os.path.join(icons_dir, "mail_icon_flagged_unread.png"),
                      "?" : os.path.join(icons_dir, "mail_icon_fetching.png")
                      },
                     "?"
                     )
    mtd.main()

if __name__ == "__main__":
    main()
