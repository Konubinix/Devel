#!/usr/bin/env python3
# -*- coding:utf-8 -*-


def sanitize_filename(value):
    import romkan
    from youtube_dl.utils import sanitize_filename
    return sanitize_filename(romkan.to_roma(value), restricted=True)
