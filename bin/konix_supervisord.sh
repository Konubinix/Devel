#!/usr/bin/env bash

mkdir -p "${TMPDIR}/supervisord"
supervisord --config "${KONIX_SUPERVISORDCONF}" "$@"
