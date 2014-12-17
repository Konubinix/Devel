#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import collections

def namedtuples_default_values(name, keys, default_values=None):
    default_values = default_values or {}
    parent = collections.namedtuple(
        name,
        keys,
    )
    res = type(
        name,
        (parent,),
        {
            "__slots__": (),
            "_fields": parent._fields,
        }
    )
    def __new__(cls, **kwarg):
        attrs = {k: default_values.get(k, None) for k in keys}
        attrs.update(kwarg)
        return parent.__new__(cls, **attrs)
    setattr(res, "__new__", __new__)
    def __str__(self):
        return self.__class__.__name__ \
            + '(\n{})'.format(
                ',\n'.join(
                    "\t{name}={value}".format(
                        name=name,
                        value=getattr(self, name),
                    )
                    for name in keys))
    res.__str__ = __str__
    return res
