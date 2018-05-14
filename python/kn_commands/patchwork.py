#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from click_project.decorators import command, argument


@command()
@argument("inputpath", nargs=-1)
@argument("outputpath")
def patchwork(inputpath, outputpath):
    import random
    from patchwork import Patchwork
    p = Patchwork()
    inputpath = list(inputpath)
    random.shuffle(inputpath)
    res = p.from_files(inputpath)
    res.save(outputpath)


if __name__ == "__main__":
    main()
