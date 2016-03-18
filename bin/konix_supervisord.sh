#!/bin/bash

mkdir -p "${TMPDIR}/supervisord"
supervisord -c "${KONIX_SUPERVISORDCONF}" "$@"
