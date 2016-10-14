#!/bin/bash

exec konix_lock_run.sh -n -N notmuch _konix_mail_sync.sh "$@"
