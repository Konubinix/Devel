#!/bin/bash

mkdir -p "${HOME}/tmp/supervisord"
supervisord -c "${KONIX_SUPERVISORDCONF}" "$@"
