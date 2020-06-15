#!/bin/bash

mkdir -p "${TMPDIR}/supervisord"
supervisord --config "${KONIX_SUPERVISORDCONF}" "$@"
