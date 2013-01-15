#!/bin/bash

if [ "$UZBL_URI" == "${UZBL_URI#https}" ]
then
    # http
    echo 'foreground="#99FF66" background="blue"'
else
    # https
    echo 'foreground="black" background="yellow"'
fi
