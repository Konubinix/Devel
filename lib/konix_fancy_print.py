#!/usr/bin/env python
# -*- coding:utf-8 -*-

import logging
try:
    import colorconsole.terminal
    colors = colorconsole.terminal.colors
    t = colorconsole.terminal.get_terminal()
    def fancy_print(foreground, background, string):
        t.cprint(foreground, background, string)
        t.reset()
except:
    logging.warning("Fancy print will not produce colorized output because the python library colorconsole was not found")
    # add colors for using scripts not to fail
    colors= { "BLACK"   : 0,
              "BLUE"    : 1,
              "GREEN"   : 2,
              "CYAN"    : 3,
              "RED"     : 4,
              "PURPLE"  : 5,
              "BROWN"   : 6,
              "LGREY"   : 7,
              "DGRAY"   : 8,
              "LBLUE"   : 9,
              "LGREEN"  : 10,
              "LCYAN"   : 11,
              "LRED"    : 12,
              "LPURPLE" : 13,
              "YELLOW"  : 14,
              "WHITE"   : 15  }
    def fancy_print(fg_color, bg_color, string):
        print string
