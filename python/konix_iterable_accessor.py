#!/usr/bin/env python
# -*- coding:utf-8 -*-

from padme import proxy
import functools

class IterableCaller(object):
    def __init__(self, list_, name):
        self.list_ = list_
        self.name = name


    def __call__(self, *args, **kwargs):
        return IterableAccessorProxy([
            getattr(object, self.name)(*args, **kwargs)
            for object in self.list_
        ])


class IterableAccessorProxy(list):
    def __init__(self, list_):
        self.l = list_
        try:
            self._first = list_[0]
        except IndexError:
            self._first = None

    def __dir__(self):
        return dir(self._first) + ["l"]

    def __repr__(self):
        return repr(self.l)

    def __iter__(self):
        return iter(self.l)

    def __getattribute__(self, name):
        if name in ["l", "_first", "__dir__", "__class__"]:
            return super(IterableAccessorProxy, self).__getattribute__(name)
        if hasattr(getattr(self._first, name), "__call__"):
            # callable, need to call it on each object
            return functools.wraps(getattr(self._first, name))(
                IterableCaller(
                    self.l,
                    name,
                )
            )
        else: # simple attribute
            return IterableAccessorProxy([
                getattr(object, name)
                for object in self.l
            ])
