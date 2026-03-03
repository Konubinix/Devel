#!/usr/bin/env bash

pgrep -f "$@" |xargs ps
