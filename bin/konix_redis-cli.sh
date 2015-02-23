#!/bin/bash

exec redis-cli -p 6380 "$@"
