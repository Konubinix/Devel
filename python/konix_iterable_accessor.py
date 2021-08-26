#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from padme import proxy
import functools
from pprint import pformat


class IterableCaller(object):
    def __init__(self, list_, name):
        self.list_ = list_
        self.name = name

    def __call__(self, *args, **kwargs):
        return IterableAccessorProxy([
            getattr(object, self.name)(*args, **kwargs)
            for object in self.list_
        ])


def powerzip(it1, it2):
    for e1, e2 in zip(it1, it2):
        res = []
        if isinstance(e1, tuple) or isinstance(e1, list):
            for elem in e1:
                res.append(elem)
        else:
            res.append(e1)
        if isinstance(e2, tuple) or isinstance(e2, list):
            for elem in e2:
                res.append(elem)
        else:
            res.append(e2)
        yield res


class IterableAccessorProxy(list):
    def __init__(self, list_):
        self.l = list_
        try:
            self._first = list_[0]
        except IndexError:
            self._first = None

    def __dir__(self):
        return dir(self._first) + ["l"]

    @property
    def df(self):
        from pandas import DataFrame
        return DataFrame(self.l)

    @property
    def series(self):
        from pandas import Series
        return Series(self.l)

    def __repr__(self):
        return "|" + pformat(self.l)

    def __iter__(self):
        return iter(self.l)

    def __sub__(self, other):
        return IterableAccessorProxy([b - a for a, b in zip(self.l, other.l)])

    def __gt__(self, other):
        return IterableAccessorProxy([a > b for a, b in zip(self.l, other.l)])

    def __lt__(self, other):
        return IterableAccessorProxy([a < b for a, b in zip(self.l, other.l)])

    def __add__(self, other):
        return IterableAccessorProxy([a + b for a, b in zip(self.l, other.l)])

    def __and__(self, other):
        return IterableAccessorProxy(list(powerzip(self.l, other.l)))

    def __getitem__(self, name):
        if isinstance(name, list):
            return IterableAccessorProxy(zip(*[getattr(self, n) for n in name]))
        elif isinstance(name, int):
            return IterableAccessorProxy([i[name] for i in self.l])
        elif isinstance(name, slice):
            return IterableAccessorProxy(
                [
                    i[slice(
                        start=name.start,
                        step=name.step or 1,
                        stop=name.stop
                    )]
                    for i in self.l
                ]
            )
        return getattr(self, name)

    def __getattribute__(self, name):
        if name in [
                "l", "__repr__", "df", "series", "_first", "__dir__", "__class__",
                "__sub__", "__add__", "__gt__", "__lt__", "__and__",
        ]:
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
