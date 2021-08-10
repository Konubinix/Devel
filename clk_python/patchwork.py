#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import click

from clk.decorators import command, argument, option


@command()
@argument("inputpath", nargs=-1, help="The images to make a patchwork from")
@argument("outputpath", help="The location of the patchwork")
@option("--width", default=1920, type=int, help="The width of the patchwork")
@option("--height", default=1080, type=int, help="The help of the patchwork")
@option("--thumbwidth",
        default=700,
        type=int,
        help="The widths of the thumbnails")
@option("--thumbheight",
        default=700,
        type=int,
        help="The heights of the thumbnails")
@option("--border", default=10, type=int, help="The size of the border")
@option("--shakefactorwidth",
        default=8,
        type=int,
        help=("The widths of the shakefactor."
              " 1 means mega shake. Increase the value to lower the shaking."))
@option("--shakefactorheight",
        default=8,
        type=int,
        help=("The heights of the shakefactor"
              " 1 means mega shake. Increase the value to lower the shaking."))
@option(
    "--dist-ratio",
    default=3 / 4,
    type=float,
    help=("The ratio of a thumbnail to use as a distance between thumbnails."
          " 1 means no overlap and 0 means too much overlap."))
@option("--type",
        type=click.Choice(["image", "video"]),
        help="Provide a type to avoid computing one on the fly")
def patchwork(inputpath, outputpath, width, height, type, dist_ratio,
              thumbheight, thumbwidth, border, shakefactorheight,
              shakefactorwidth):
    """Create a patchwork out of several images"""
    import random
    from patchwork import Patchwork
    p = Patchwork(
        width=width,
        height=height,
        thumbwidth=thumbwidth,
        thumbheight=thumbheight,
        dist_ratio=dist_ratio,
        shakefactorwidth=shakefactorwidth,
        shakefactorheight=shakefactorheight,
        border=border,
    )
    inputpath = list(inputpath)
    random.shuffle(inputpath)
    res = p.from_files(inputpath, type=type)
    res.save(outputpath)
