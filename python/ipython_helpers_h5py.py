#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import h5py
import re

def hdf_keys(hdf_file, recursive=False, leaves_only=False):
    import h5py
    file_ = h5py.File(hdf_file, mode="r")
    keys = set()
    if recursive:
        def handle_key(e):
            if not leaves_only or isinstance(file_[e], h5py._hl.dataset.Dataset):
                e = re.sub("^(.+)/(index|values)$", r"\1", e)
                keys.add(e)
        file_.visit(handle_key)
    else:
        keys = set(file_)
    file_.close()
    return keys
