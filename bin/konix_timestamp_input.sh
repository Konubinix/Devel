#!/bin/bash

FORMAT="$*"

while read line
do
    date "+$FORMAT $line"
done
