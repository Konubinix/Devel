#!/bin/bash


find -name '*.JPG' -o -name '*.jpg' | parallel konix_image_rename_rotate.sh
