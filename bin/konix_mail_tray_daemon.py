#!/usr/bin/env python
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
                          os.path.join(icons_dir, "mail_icon_empty.png"),
                          os.path.join(icons_dir, "mail_icon_new_mail.png")
                          )
    mtd.main()

if __name__ == "__main__":
    main()
