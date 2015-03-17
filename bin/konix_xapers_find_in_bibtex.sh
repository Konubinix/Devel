#!/bin/bash

cd "${XAPERS_ROOT}"
find -name bibtex -exec konix_xapers_find_in_bibtex_exec.sh '{}' "$@" ';'|sed -r 's|./0+|id:|'
