#!/bin/bash

readelf -a "$1" | grep RPATH
