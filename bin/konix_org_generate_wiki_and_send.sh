#!/bin/bash

export PATH="$PATH:/home/sam/Prog/devel/bin"
source konix_assert_var.sh "$KONIX_WIKI_DIR"
source konix_assert_var.sh "$KONIX_SITE_LOGIN"
source konix_assert_var.sh "$KONIX_SITE_MDP"
source konix_assert_var.sh "$KONIX_SITE_SERVER"

emacsclient -e "(save-excursion (konix/org-wiki-generate))"
# the org file is by default in ~/org.ics
(
	cd "${KONIX_WIKI_DIR}"
	lftp -e "cd public_html; mkdir wiki ; cd wiki ; mirror -R ; quit" "$KONIX_SITE_LOGIN:$KONIX_SITE_MDP@$KONIX_SITE_SERVER"
)
