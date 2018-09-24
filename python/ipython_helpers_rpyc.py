#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import threading
import rpyc

def start_in_thread(*objs):

    def function(*objs):
        from prompt_toolkit.interface import _StdoutProxy
        _StdoutProxy.isatty = lambda self: False
        import rpyc_classic
        rpyc_classic.ClassicServer()

    return threading.Thread(target=function, args=objs)
