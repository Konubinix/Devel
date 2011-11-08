#!/bin/bash

find . | awk '/~$/ {print}' | xargs rm && echo "finished cleaning emacs auto save files" &
