#!/bin/bash -eu

dir="$1"
for cal in "${dir}"/*ics
do
    konix_ical_radicalize.py "${cal}" && rm "${cal}"
done
