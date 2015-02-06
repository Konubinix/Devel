#!/bin/bash

if ! which hypermail 2> /dev/null >&2
then
    echo "Install hypermail from http://www.hypermail-project.org/"
    exit 1
fi

if ! which formail 2> /dev/null >&2
then
    echo "formail missing: sudo aptitude install procmail"
    exit 1
fi

OUTPUT="$(pwd)/html_output"
> ~/tmp/mbox
trap "rm ~/tmp/mbox" 0
notmuch search \
    --output=files \
    "$@" \
|while read i
do
    formail < "$i" >> ~/tmp/mbox
done

hypermail -c "${HYPERMAIL_CONFIG}" -m ~/tmp/mbox -d "${OUTPUT}"
echo "HTML written in ${OUTPUT}"
