#!/bin/bash

if [ -z "$1" ]
then
	set `zenity --entry --text "Search what ?"`
fi
"$BROWSER" "http://localhost:8250/search?q=$*&expansion=1&action=expand"
