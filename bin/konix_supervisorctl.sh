#!/usr/bin/env bash

supervisorctl --config "${KONIX_SUPERVISORDCONF}" -r "${KONIX_SUPERVISORCTL_HISTORY}" "$@"
