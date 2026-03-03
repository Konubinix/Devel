#!/usr/bin/env bash

netstat -tupln| grep -i "$@"
