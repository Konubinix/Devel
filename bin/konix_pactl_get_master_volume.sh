#!/bin/bash

pactl list sinks |sed -nr '/Volume: front-left:/ {
 s/^.+ ([0-9]+)%.+/\1/ p }'
