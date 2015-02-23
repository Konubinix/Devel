#!/bin/bash

exec redis-server "${KONIX_REDIS_CONF}" "$@"
