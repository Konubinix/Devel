#!/bin/bash
#set -x
source konix_assert_var.sh "$KONIX_FANCY_ORG_NAME"
source konix_assert_var.sh "$KONIX_SITE_LOGIN"
source konix_assert_var.sh "$KONIX_SITE_MDP"
source konix_assert_var.sh "$KONIX_SITE_SERVER"

emacsclient -e "(save-excursion (org-mycal-export))"
# the org file is by default in ~/org.ics
(
	cd "${HOME}"
	lftp -e "cd public_html ; put org.ics ; mv org.ics $KONIX_FANCY_ORG_NAME ; quit" "$KONIX_SITE_LOGIN:$KONIX_SITE_MDP@$KONIX_SITE_SERVER"
)
