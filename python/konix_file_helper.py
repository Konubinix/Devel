#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from slugify import slugify


def sanitize_filename(name, comparable=False):
    """Make a name a file may carry.

    Ask for it comparable to get the name two spellings of one thing agree on:
    case and punctuation go, so that Rondo. Allegro meets Rondo Allegro and
    VI. meets Vi.
    """
    return slugify(
        name,
        lowercase=comparable,
        regex_pattern="[^a-zA-Z0-9]+" if comparable else "[^-a-zA-Z0-9_.]+",
    )
