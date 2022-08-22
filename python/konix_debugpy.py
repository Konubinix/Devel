#!/usr/bin/env python3
# -*- coding:utf-8 -*-


def set_trace():
    import debugpy
    debugpy.listen(5678)
    debugpy.wait_for_client()
