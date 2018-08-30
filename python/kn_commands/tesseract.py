#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
from shlex import split

from click_project.decorators import command, argument, option
from click_project.lib import call, temporary_file


@command()
@argument("input", help="Input file to convert")
@option("--language", default="fra", help="Language of the input file")
def tesseract(input, language):
    """Extract the text from a pdf and create a searchable pdf"""
    ocr_base = os.path.splitext(input)[0] + "_ocr"
    with temporary_file(suffix=".tiff") as tiff:
        call(split(f"convert -density 300 '{input}' -depth 8 '{tiff.name}'"))
        call(split(f"tesseract '{tiff.name}' '{ocr_base}' -l '{language}' pdf"))


if __name__ == "__main__":
    main()
