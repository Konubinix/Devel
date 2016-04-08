#!/usr/bin/env python
# -*- coding:utf-8 -*-

import pipe
import IPython
ip = IPython.get_ipython()

_piped_funcs = set(
    [s
     for s in dir(pipe)
     if (
             hasattr(getattr(pipe, s), '__ror__')
             and s != 'Pipe'
     )
 ]
    )
class _newPipe(pipe.Pipe, object):
    def __init__(self, function):
        super(_newPipe, self).__init__(function)
        _piped_funcs.add(function.__name__)

    def __ror__(self, other):
        self.res = self.function(other)
        return self.res

pipe.Pipe = _newPipe
def _pipe_completer(self, event):
    import re
    symbol = re.match('(?P<symbol>.+\\|)', event.symbol).group("symbol")
    return [symbol + name for name in _piped_funcs]

def _greedy_completer(self, event):
    import re
    symbol = re.match('(?P<symbol>.+\\.)', event.symbol).group("symbol")
    line = re.match('(?P<line>.+)\\.', event.line).group("line")
    try:
        obj = eval(line, ip.user_global_ns, ip.user_ns)
    except Exception, e:
        print e
    return [symbol + s for s in dir(obj)]

ip.set_hook('complete_command', _greedy_completer, re_key = '.+\\|.+?\\.[a-zA-Z_-]*$')
ip.set_hook('complete_command', _pipe_completer, re_key = '.+\\|[a-zA-Z_-]*$')
