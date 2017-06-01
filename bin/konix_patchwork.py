#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from PIL import Image, ImageOps
import sys
import random
import itertools


def main():
    imagepaths = sys.argv[1:-1]
    finalpath = sys.argv[-1]
    random.shuffle(imagepaths)
    width = 1920
    height = 1080
    thumbwidth = 700
    thumbheight = 700
    background_color = 300
    rotation = 15
    b = Image.new("RGB", (width, height), background_color)
    coordinates = itertools.product(
        range(-thumbwidth // 2, width, thumbwidth // 2),
        range(-thumbheight // 2, height, thumbheight // 2),
    )
    values = list(zip(coordinates, itertools.cycle(imagepaths)))
    random.shuffle(values)
    for (x, y), imagepath in values:
        dx = random.randint(-thumbwidth // 8, thumbwidth // 8)
        dy = random.randint(-thumbheight // 8, thumbheight // 8)
        nx = x + dx
        ny = y + dy
        print("Handling {} at ({}, {}) -> ({}, {})".format(
            imagepath, x, y, nx, ny))
        i = ImageOps.expand(Image.open(imagepath).convert("RGBA"), 10,
                            (255,
                             255,
                             255))
        i.thumbnail((thumbwidth, thumbheight))
        i = i.rotate(random.randint(-rotation, rotation), expand=True)
        b.paste(i, (nx, ny), i)
        b.save(finalpath)
    b.save(finalpath)


if __name__ == "__main__":
    main()
