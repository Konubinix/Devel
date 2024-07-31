#!/bin/bash

echo "############"
date
echo "############"

TMPDIR="$(mktemp -d)"
trap "rm -rf '${TMPDIR}'" 0
keep_color () {
	unbuffer "$@"
}
# set -x
pushd "${KONIX_FEED_IMAP_FLEXGET_DIR}"
keep_color flexget execute | tee "${TMPDIR}/log"
popd

CYAN="$(tput setaf 6)"
RED="$(tput setaf 1)"
GREEN="$(tput setaf 2)"
OFF="$(tput sgr0)"
BOL="$(tput cr)"
KILL_LINE="$(tput el)"


cat "${TMPDIR}/log" \
    | ansi2txt \
    | grep "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] \(ERROR\|CRITICAL\|WARNING\)" \
    | grep -v 'failed to import dependencies'   \
    | grep -v 'Skipped %s RSS-entries without required information'  \
           > "${TMPDIR}/errors"
if test "$(wc -l < "${TMPDIR}/errors")" = "0"
then
    konix_display.py -o "Ended getting rss feeds. No error."
else
    msg="Ended getting rss feeds ${RED}$(wc -l < "${TMPDIR}/errors")${OFF} problems"
    konix_display.py -o $(echo $msg|ansi2txt)
    echo "${msg}"
    echo "${RED}$(cat "${TMPDIR}/errors")${OFF}"
fi
# init the tags for the new mails
echo "Initing tags"
konix_mail_new.sh
echo "Ended init tags"
