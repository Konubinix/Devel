#!/bin/bash

export LC_ALL=C

aptitude -O installsize search ~i -F '%I | %p' --disable-columns|sed -r 's/^([0-9]+) ([a-zA-Z]+)(.*)$/\1\2\3/'|sort -h
