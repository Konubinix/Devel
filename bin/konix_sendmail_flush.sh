#!/bin/bash -eu
mkdir -p "${TMPDIR}/logs/"
LOG="${TMPDIR}/logs/msmtp.log"
rm -f "${LOG}"

QUEUE_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/mail.queue"
mkdir -p "${QUEUE_DIR}"

msg () {
    echo "$*"
    clk ntfy "$*"
}

if test 0 = $(ls "${QUEUE_DIR}" | wc -l)
then
    echo "Nothing to send"
else
    TIMETOWAIT=10
    echo "Waiting ${TIMETOWAIT}s before sending, just in case"
    sleep ${TIMETOWAIT}
    echo "Sending now!!"
    trap "test -e ${LOG} && cat ${LOG}" 0
    clk gpg decrypt dir "${KONIX_PERSO_DIR}/msmtprc_nd" msmtprc
    msmtpq-flush || {
        clk ntfy --priority 5 "Could not send a mail. This is important!!"
        exit 2
    }
    if grep -q auth=off "${LOG}"
    then
        msg "Mail most likely not sent, because using some unknown account, check the logs at ${LOG}"
    else
        msg "Mail sent"
    fi
    notmuch new
fi
