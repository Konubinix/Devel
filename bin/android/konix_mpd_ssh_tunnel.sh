#!/bin/bash

exec ssh -f -L9635:127.0.0.1:9635 -L6660:127.0.0.1:6600 "${MPD_HOST}" sleep 5000000000
