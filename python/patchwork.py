#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import itertools
import logging
import mimetypes
import random
from pathlib import Path

import numpy as np
import requests
from clk.lib import temporary_file
from moviepy.editor import VideoFileClip
from PIL import Image, ImageOps

LOGGER = logging.getLogger(__name__)


class Patchwork():

    def __init__(self,
                 width=1920,
                 height=1080,
                 thumbwidth=700,
                 thumbheight=700,
                 dist_ratio=3. / 4.,
                 background_color=300,
                 rotation=15,
                 shakefactorwidth=8,
                 shakefactorheight=8,
                 border=10):
        self.width = width
        self.height = height
        self.border = border
        self.thumbwidth = thumbwidth
        self.thumbheight = thumbheight
        self.dist_ratio = dist_ratio
        self.shakefactorwidth = shakefactorwidth
        self.shakefactorheight = shakefactorheight
        self.background_color = background_color
        self.rotation = rotation
        self.coordinates = itertools.product(
            range(int(-self.thumbwidth * self.dist_ratio), self.width,
                  int(self.thumbwidth * self.dist_ratio)),
            range(int(-self.thumbheight * self.dist_ratio), self.height,
                  int(self.thumbheight * self.dist_ratio)),
        )

    def compute_thumbnails(self, paths, type=None):
        with temporary_file(nameonly=True) as f:
            f.close()
            for path in paths:
                if path.startswith("http"):
                    print(f"Getting {path}")
                    Path(f.name).write_bytes(requests.get(path).content)
                    path = f.name

                type = type or mimetypes.guess_type(path)[0].split("/")[0]
                if type == "image":
                    i = ImageOps.expand(
                        Image.open(path).convert("RGBA"), self.border,
                        (255, 255, 255))
                    i.thumbnail((self.thumbwidth, self.thumbheight))
                    yield i
                elif type == "video":
                    clip = VideoFileClip(path)
                    fps = 10 / clip.duration
                    tt = np.arange(0, clip.duration, 1.0 / fps)
                    for t in tt:
                        array = clip.get_frame(t)
                        i = ImageOps.expand(
                            Image.fromarray(array).convert("RGBA"),
                            self.border, (255, 255, 255))
                        i.thumbnail((self.thumbwidth, self.thumbheight))
                        yield i
                else:
                    raise NotImplementedError(
                        "Cannot compute an image from '{}' of type '{}'".
                        format(path, type))

    def from_files(self, imagepaths, type=None):
        thumbnails = self.compute_thumbnails(imagepaths, type=type)
        return self.from_images(thumbnails)

    def from_images(self, images):
        b = Image.new("RGB", (self.width, self.height), self.background_color)
        values = list(zip(self.coordinates, itertools.cycle(images)))
        random.shuffle(values)
        for (x, y), i in values:
            dx = random.randint(-self.thumbwidth // self.shakefactorwidth,
                                self.thumbwidth // self.shakefactorwidth)
            dy = random.randint(-self.thumbheight // self.shakefactorheight,
                                self.thumbheight // self.shakefactorheight)
            nx = x + dx
            ny = y + dy
            i = i.copy()
            i = i.rotate(
                random.randint(-self.rotation, self.rotation),
                resample=Image.BILINEAR,
                expand=True,
            )
            b.paste(i, (nx, ny), i)
        return b
