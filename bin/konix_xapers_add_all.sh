#!/bin/bash

export KONIX_XAPERS_ADD_TAGS="$(konix_gtk_entry.py -t Tags -n -i "${KONIX_XAPERS_ADD_TAGS}")"
while konix_xapers_add_next.sh
do
    xapers count
done
