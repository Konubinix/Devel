#!/bin/bash

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
mkdir -p "${HOME}/cryptoff/.pymsmtpq.queue"
msmtpq -t --read-envelope-from -oi -C "${KONIX_PERSO_DIR}/msmtprc/config" --aliases="${KONIX_MSMTP_ALIASES}" --logfile "${KONIX_MSMTP_LOG}" "$@"
