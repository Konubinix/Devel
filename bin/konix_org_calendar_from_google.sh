#!/bin/bash

source konix_assert_var.sh "$KONIX_ICSFILE"
source konix_assert_var.sh "$KONIX_ORGFILE"
source konix_assert_var.sh "$KONIX_GOOGLE_CAL_URL"

rm -f "$KONIX_ICSFILE"
wget -O "$KONIX_ICSFILE" "$KONIX_GOOGLE_CAL_URL"
ical2org < $KONIX_ICSFILE > $KONIX_ORGFILE
