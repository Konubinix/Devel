#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
from shlex import split

from click_project.decorators import command, argument, option, flag
from click_project.lib import call, temporary_file, rm


@command()
@argument("input", help="Input file to convert")
@option("--language", default="fra", help="Language of the input file")
@flag("--remove-after/--keep-original",
      default=True,
      help="Remove the original file when everything went well")
def tesseract(input, language, remove_after):
    """Extract the text from a pdf and create a searchable pdf"""
    ocr_base = os.path.splitext(input)[0] + "_ocr"
    with temporary_file(suffix=".tiff") as tiff:
        call(split(f"convert -density 300 '{input}' -depth 8 '{tiff.name}'"))
        call(split(f"tesseract '{tiff.name}' '{ocr_base}' -l '{language}' pdf"))
    if remove_after:
        rm(input)


if __name__ == "__main__":
    main()
