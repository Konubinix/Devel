#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import konix_gcalib, sys

if __name__ == "__main__":
    program = konix_gcalib.GCall()
    if len(sys.argv) > 1:
        program.onecmd(sys.argv[1] + ' "' + '" "'.join(sys.argv[2:]) + '"')
    else:
        program.cmdloop()
