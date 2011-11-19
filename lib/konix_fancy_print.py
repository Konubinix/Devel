#!/usr/bin/env python
# -*- coding:utf-8 -*-

import logging
try:
    import colorconsole.terminal
    t = colorconsole.terminal.get_terminal()
    fancy_print = t.cprint
except:
    logging.warning("Fancy print will not produce colorized output because the python library colorconsole was not found")
    def fancy_print(fg_color, bg_color, string):
        print string
