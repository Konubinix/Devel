#!/usr/bin/env bash

exec redis-server "${KONIX_REDIS_CONF}" "$@"
