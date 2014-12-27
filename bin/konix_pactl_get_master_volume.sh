#!/bin/bash

pactl list sinks |sed -nr '/Volume: [^:]+:/ {
 s/^.+ ([0-9]+)%.+/\1/ p }'
