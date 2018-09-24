#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from ipython_helpers_rpyc import start_in_thread as _start_in_thread
import bpy


def get_active_object():
    return bpy.context.active_object


bpy.get_active_object = get_active_object


def start_in_thread():
    _start_in_thread().start()
