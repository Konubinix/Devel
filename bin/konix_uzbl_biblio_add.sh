#!/bin/bash

set -eu
URI="${1}"
konix_display.py "Adding ${URI} to the biblio"
RES="$(konix_biblio_add.sh "${URI}" 2>&1)"
if [ "$?" == "0" ]
then
    konix_display.py "Added ${URI} to the biblio with result
$RES"
else
    konix_display.py "FAILED TO ADD ${URI} TO THE BIBLIO
$RES"
fi
