#!/bin/bash

EXE=`dmenu_path | konix_dmenu.sh`
if [ -n "${EXE}" ]
then
    exec "${EXE}"
fi
