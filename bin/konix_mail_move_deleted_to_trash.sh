#!/bin/bash

konix_notmuch_deleted_not_trashed.sh -f | grep "Gmail" | while read line
do
    konix_gmailmaildir_moveto_trash.py "$line"
done
