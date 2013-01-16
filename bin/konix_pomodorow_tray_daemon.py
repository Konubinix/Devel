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
                                       "pomodorow_tray_daemon_control"),
                          200,
                     {"i" : os.path.join(icons_dir, "pomodorow_idle.png"),
                      "r" : os.path.join(icons_dir, "pomodorow_rings.png"),
                      "b" : os.path.join(icons_dir, "pomodorow_end_break.png"),
                      "c" : os.path.join(icons_dir, "pomodorow_clock_in.png"),
                      "j" : os.path.join(icons_dir, "pomodorow_interruption.png")
                      },
                     "i"
                     )
    mtd.main()

if __name__ == "__main__":
    main()
