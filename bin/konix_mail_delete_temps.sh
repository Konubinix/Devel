#!/bin/bash -x

NUMBER_OF_MONTH="${KONIX_TEMP_MAIL_KEEP_MONTHS:-3}"
notmuch tag +deleted -- tag:temp and not tag:unread and not tag:flagged and .."$(konix_last_month_since_epoch.sh $NUMBER_OF_MONTH)"
