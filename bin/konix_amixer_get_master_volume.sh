#!/usr/bin/env bash

amixer get Master | sed -rn '/Front Left:/ {
s/^[^[]+\[([0-9]+).+/\1/ p
}
'
