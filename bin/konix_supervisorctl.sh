#!/bin/bash

supervisorctl -r "${KONIX_SUPERVISORCTL_HISTORY}" "$@"
