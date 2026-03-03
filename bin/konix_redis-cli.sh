#!/usr/bin/env bash

echo "## konix_redis-cli.sh IS DEPRECATED, USE REDIS-CLI" >&2
exec redis-cli "$@"
