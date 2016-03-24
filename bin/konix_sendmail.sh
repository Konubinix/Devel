#!/bin/bash

# heilroom gives -r to specify the sender while msmtp expects -a or -f
# find the position of a -r
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
#echo $found
#echo "$@"
# took from http://stackoverflow.com/questions/4827690/change-a-command-line-argument-bash
if [ "$found" != "" ]
then
    if [ "$found" -gt "0" ]
    then
        set -- "${@:1:${found}}" "-a" "${@:$((found + 2 ))}"
    else
        set -- "-a" "${@:2}"
    fi
fi
#echo "$@"
#xmessage "$*"
mkdir -p "${HOME}/cryptoff/.pymsmtpq.queue"
pymsmtpq -t --read-envelope-from -oi -C "${KONIX_PERSO_DIR}/msmtprc/config" --aliases="${KONIX_MSMTP_ALIASES}" --logfile "${KONIX_MSMTP_LOG}" "$@"
