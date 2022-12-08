#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from slugify import slugify


def sanitize_filename(name):
    return slugify(
        name,
        lowercase=False,
        regex_pattern="[^-a-zA-Z0-9_.]+",
    )
