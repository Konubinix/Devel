#!/usr/bin/env bash

# When given the -r option, heilroom while add the From field in the envelope
# and pass this -r option through to the msmtp call. msmtp does not recognize
# the -r option, and because here we use the --read-envelope-from, it while find
# out the correct account, reading the From field.

# As a conclusion, we just need to get rid of this extra -r option

pos=0
found=""
for arg in "$@"
do
    if [ "$arg" == "-r" ] ||  [ "$arg" == "-f" ]
    then
        found="$pos"
    fi
    pos=$((pos + 1))
done

# inspired from http://stackoverflow.com/questions/4827690/change-a-command-line-argument-bash
#echo "$@"
if [ "$found" != "" ]
then
    if [ "$found" -gt "0" ]
    then
        set -- "${@:1:${found}}" "${@:$((found + 3 ))}"
    else
        set -- "${@:3}"
    fi
fi
#echo "$@"
#xmessage "$*"
clk gpg decrypt dir "${KONIX_PERSO_DIR}/msmtprc_nd" msmtprc
LOGFILE="$(mktemp)"
trap "rm -f ${LOGFILE}" 0
# msmtpq uses set -o errexit and flushes its queue after sending, so it may
# return non-zero even when the current mail was sent successfully. Use the
# msmtp logfile (which records exitcode=EX_OK on success) as the source of
# truth instead.
msmtpq -t --read-envelope-from -oi -C "/run/user/$(id -u)/msmtprc/config" --aliases="${KONIX_MSMTP_ALIASES}" --logfile="${LOGFILE}" "$@" > /dev/null 2>&1 || true
if ! grep -q "exitcode=EX_OK" "${LOGFILE}" 2>/dev/null
then
    cat "${LOGFILE}"
    exit 1
fi
