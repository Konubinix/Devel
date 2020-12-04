#!/bin/bash

NUMBER_OF_MONTH="${KONIX_TEMP_MAIL_KEEP_MONTHS:-3}"
SAFE=$1

# Get old threads. an old thread is a thread with all its messages older than
# KONIX_TEMP_MAIL_KEEP_MONTHS months. Get only those among the mails that may be
# deleted, that means only the temp mails with not any mail flagged and all
# mails read. The threads result will look like
# thread:...    ... [A/B] ...
# If all mails of the thread match the condition, then A == B, in that case, and
# only in that case, I can delete the whole thread. If there is one mail in the
# thread that does not match all the conditions (temporary and read and not
# flagged and old enough), then A != B
TMP_FILE="$(mktemp -t delete_temp_threads.XXXX)"
trap -- "rm $TMP_FILE" 0

eval_notmuch_on_threads () {
    NOTMUCH_COMMAND="$*"
    LINE_NUMBER=0
    ARGS=""
    #    set -x
    while read line
    do
        if [ "$ARGS" == "" ]
        then
            ARGS="$line"
        else
            ARGS="$ARGS or $line"
        fi
        if let "$LINE_NUMBER >= 50"
        then
            $NOTMUCH_COMMAND $ARGS
            ARGS=""
            LINE_NUMBER=0
        fi
        LINE_NUMBER="$((LINE_NUMBER + 1))"
    done
    if [ "$ARGS" != "" ]
    then
        $NOTMUCH_COMMAND $ARGS
    fi
}

TIMESTAMP="$(konix_last_month_since_epoch.sh $NUMBER_OF_MONTH)"
notmuch search \
    --sort=oldest-first \
    tag:temp \
    and not tag:unread \
    and not tag:flagged \
    and not tag:deleted \
    and not tag:sent \
    and ..$TIMESTAMP \
    | sed -n '/\[\([0-9]\+\)\/\1\]/ { # show only threads matching [A/A] and not [A/B] with A != B
# display only the thread id part
s/^\(thread:[^ ]\+\).\+/\1/
p
}
'>"${TMP_FILE}"

safe_delete () {
    while [ "$(wc -l "${TMP_FILE}" |cut -f 1 -d' ')" != "0" ]
    do
        sed -n "1,50 p" "${TMP_FILE}" |eval_notmuch_on_threads notmuch search --|tac
        echo "Those mails will be put to be deleted, confirm?"
        read y
        if [ "$y" == "y" ]
        then
            echo "Flagging them as deleted"
            head -50 "${TMP_FILE}"|eval_notmuch_on_threads notmuch tag +deleted --
        fi
        sed -i -n '51,$ p' "${TMP_FILE}"
    done
}

all_delete () {
    cat "${TMP_FILE}"|eval_notmuch_on_threads notmuch search --
    echo "Those mails will be put to be deleted, confirm?"
    read y
    if [ "$y" == "y" ]
    then
        cat "${TMP_FILE}"|eval_notmuch_on_threads notmuch tag +deleted --
    fi
}

echo "Considering $(wc -l "${TMP_FILE}") mails"

if [ "$SAFE" == "" ]
then
    all_delete
else
    safe_delete
fi
