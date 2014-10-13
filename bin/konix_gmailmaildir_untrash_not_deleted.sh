#!/bin/bash

konix_notmuch_trashed_not_deleted.sh -f | while read line
do
    konix_gmailmaildir_untrash.py "${line}"
done
