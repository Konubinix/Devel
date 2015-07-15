#!/usr/bin/env python2
# -*- coding:utf-8 -*-

import os
import tempfile

from konix_tray_daemon import TrayDaemon

EMACS_TRAY_PERIOD = 200

if __name__ == "__main__":
    share_dir = os.environ.get("KONIX_SHARE_DIR",
                               os.path.join(os.environ["HOME"],"share"))
    icons_dir = os.path.join(share_dir, "icons")

    etd = TrayDaemon(os.path.join(tempfile.gettempdir(),
                                  "emacs_tray_daemon_control"),
                     EMACS_TRAY_PERIOD,
                     {"i" : os.path.join(icons_dir, "emacs_icon_blue.png"),
                      "n" : os.path.join(icons_dir, "emacs_icon_yellow.png"),
                      "N" : os.path.join(icons_dir, "emacs_icon_red.png"),
                      "D" : os.path.join(icons_dir, "emacs_icon_black.png")
                      },
                     "i"
                     )
    etd.main()
