#!/usr/bin/env python
# -*- coding:utf-8 -*-

def hdf_keys(hdf_file, recursive=False):
    import h5py
    file_ = h5py.File(hdf_file, mode="r")
    keys = []
    if recursive:
        file_.visit(lambda e: keys.append(e))
    else:
        keys = list(file_)
    file_.close()
    return keys
