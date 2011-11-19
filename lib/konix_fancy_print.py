#!/usr/bin/env python
# -*- coding:utf-8 -*-

import logging
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
try:
    import colorconsole.terminal
    t = colorconsole.terminal.get_terminal()
    fancy_print = t.cprint
except:
    logging.warning("Fancy print will not produce colorized output because the python library colorconsole was not found")
    def fancy_print(fg_color, bg_color, string):
        print string
