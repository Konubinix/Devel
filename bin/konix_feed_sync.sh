#!/bin/bash

echo "############"
date
echo "############"

TMPDIR="$(mktemp -d)"
trap "rm -rf '${TMPDIR}'" 0
keep_color () {
	unbuffer "$@"
}
set -x
pushd "${KONIX_FEED_IMAP_FLEXGET_DIR}"
keep_color flexget execute | tee "${TMPDIR}/log"
popd


keep_color grep "ERROR\|CRITICAL\|WARNING" "${TMPDIR}/log" | tee "${TMPDIR}/errors"
konix_display.py -o "Ended getting rss feeds $(wc -l < "${TMPDIR}/errors") problems"

# init the tags for the new mails
echo "Initing tags"
konix_mail_new.sh
echo "Ended init tags"
