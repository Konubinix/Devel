#!/bin/bash

pgrep -f "$@" |xargs ps
