#!/bin/bash -eu

find -name .git -prune -o -\( -name '*.JPG' -o -name '*.jpg' -\) -print | parallel konix_image_reorient.sh '{}' ';'
