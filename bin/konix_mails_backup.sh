#!/bin/bash

find "${HOME}/Mail" -name cur -exec konix_maildir_backup.sh {}/.. "${HOME}/Mailbackups/" ';'
