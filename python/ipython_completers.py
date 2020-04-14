#!/usr/bin/env python
# -*- coding:utf-8 -*-

import IPython
ip = IPython.get_ipython()

# import pipe
# def _pipe_completer(self, event):
#     import re
#     symbol = re.match('(?P<symbol>.+\\|)', event.symbol).group("symbol")
#     return [symbol + function.__name__ for function in pipe.pipe_functions]
#ip.set_hook('complete_command', _pipe_completer, re_key = '.+\\|[a-zA-Z_-]*$')

def _greedy_completer(self, event):
    import re
    symbol = re.match('(?P<symbol>.+\\.)', event.symbol).group("symbol")
    line = re.match('(?P<line>.+)\\.', event.line).group("line")
    try:
        obj = eval(line, ip.user_global_ns, ip.user_ns)
    except Exception as e:
        print(e)
        return []
    return [symbol + s for s in dir(obj)]

ip.set_hook('complete_command', _greedy_completer, re_key = '.+\\|.+?\\.[a-zA-Z_-]*$')
