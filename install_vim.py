#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from install_lib import *

def install_vim():
    environ = get_environ()
    replace_file_content(os.path.join(environ["HOME"] , ".vimrc"),
                         "source %(KONIX_CONFIG_DIR)s/vimrc" % environ)

    print("Successfully installed vim config")

if __name__ == "__main__":
    install_vim()
