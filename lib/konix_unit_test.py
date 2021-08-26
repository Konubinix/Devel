#!/usr/bin/env python3
# -*- coding:utf-8 -*-
import os

class TestFixtureFilesBuilder(object):
    """A class to easily create some files for unit tests.
    Use it like that :
    >>> TestFixtureFilesBuilder(
    ... [
    ...  (
    ...   filename,
    ...   file_content
    ...  ),
    ...  (
    ...   filename2,
    ...   file_content2
    ...  )
    ... ])

    The filenames must be relative names, they will be put in a temporary directory.
    The relative directories will automatically be created.
    When destroying the object, the temp directory will also be automatically deleted
    """
    def __init__(self, item_file_list):
        import tempfile
        self.folder = tempfile.mkdtemp()
        self.file_list = item_file_list
        for item_file_name, item_content in self.file_list:
            item_file_name = os.path.join(self.folder, item_file_name)
            if not os.path.exists(os.path.dirname(item_file_name)):
                os.makedirs(os.path.dirname(item_file_name))
            item_file = open(item_file_name, "w")
            item_file.write(item_content)
            item_file.close()

    def __del__(self):
        import shutil
        shutil.rmtree(self.folder)
