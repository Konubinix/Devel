#!/usr/bin/env python3
# -*- coding:utf-8 -*-

class DictMapper(object):
    def __init__(self, dict_):
        self.dict_ = dict_

    def __dir__(self):
        return ["dict_"] + list(self.dict_.keys())

    def __getattr__(self, key):
        if key == "dict_":
            return super(DictMapper, self).__getattr__(key)
        value = self.dict_[key]
        if isinstance(value, dict) or type(value) == dict:
            return DictMapper(value)
        return value
