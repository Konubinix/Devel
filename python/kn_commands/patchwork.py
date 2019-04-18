#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from click_project.decorators import command, argument, option


@command()
@argument("inputpath", nargs=-1, help="The images to make a patchwork from")
@argument("outputpath", help="The location of the patchwork")
@option("--width", default=1920, type=int, help="The width of the patchwork")
@option("--height", default=1080, type=int, help="The help of the patchwork")
def patchwork(inputpath, outputpath, width, height):
    """Create a patchwork out of several images"""
    import random
    from patchwork import Patchwork
    p = Patchwork(width=width, height=height)
    inputpath = list(inputpath)
    random.shuffle(inputpath)
    res = p.from_files(inputpath)
    res.save(outputpath)
