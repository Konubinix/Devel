#!/usr/bin/env python
# -*- coding:utf-8 -*-

import pipe
import IPython
ip = IPython.get_ipython()

def _pipe_completer(self, event):
    import re
    symbol = re.match('(?P<symbol>.+\\|)', event.symbol).group("symbol")
    return [symbol + function.__name__ for function in pipe.pipe_functions]

def _greedy_completer(self, event):
    import re
    symbol = re.match('(?P<symbol>.+\\.)', event.symbol).group("symbol")
    line = re.match('(?P<line>.+)\\.', event.line).group("line")
    try:
        obj = eval(line, ip.user_global_ns, ip.user_ns)
    except Exception(e):
        print(e)
    return [symbol + s for s in dir(obj)]

ip.set_hook('complete_command', _greedy_completer, re_key = '.+\\|.+?\\.[a-zA-Z_-]*$')
ip.set_hook('complete_command', _pipe_completer, re_key = '.+\\|[a-zA-Z_-]*$')
