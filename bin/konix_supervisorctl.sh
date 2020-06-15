#!/bin/bash

supervisorctl --config "${KONIX_SUPERVISORDCONF}" -r "${KONIX_SUPERVISORCTL_HISTORY}" "$@"
