#!/bin/bash

find -name '*.JPG' -o -name '*.jpg' -exec "konix_image_rename.sh" "{}" ";"
