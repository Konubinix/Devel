#!/bin/bash

read title
echo -n "* NEXT ${title}
:PROPERTIES:
:CREATED: [$(date '+%Y-%m-%d %a %H:%M')]
:END:
" >>  "${KONIX_PERSO_DIR}/wiki/inbox.org"
