#!/bin/bash

# inspired from http://askubuntu.com/questions/27894/get-window-size-in-shell
export id=$(xdotool getactivewindow|xargs printf "0x%.8x\n")
wmctrl -l $@ | grep "$id"
