#!/bin/bash -eu

dir="$1"
destination="$2"
for cal in "${dir}"/*ics
do
    clk python $(which konix_ical_radicalize.py) "${cal}" "${destination}" # && rm "${cal}"
done
