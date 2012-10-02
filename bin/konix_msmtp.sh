#!/bin/bash

msmtp -t -oi -C "${KONIX_PERSO_DIR}/msmtprc" --logfile "$HOME/msmtp_log" "$@"
