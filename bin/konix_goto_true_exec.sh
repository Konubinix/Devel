#!/bin/bash

TRUE_EXEC="$(konix_find_true_exec.sh "$1")"
if [ -z "$TRUE_EXEC" ]
then
	konix_display.py "Cannot find true exec for $1"
else
	konix_goto_window.sh "$TRUE_EXEC"
fi
