#!/usr/bin/env bash

readelf -a "$1" | grep RPATH
