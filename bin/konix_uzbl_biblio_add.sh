#!/bin/bash

set -eu
URI="${1}"
konix_display.py "Adding ${URI} to the biblio"
konix_biblio_add.sh "${URI}" && \
    konix_display.py "Added ${URI} to the biblio" || \
        konix_display.py "FAILED TO ADD ${URI} TO THE BIBLIO"
