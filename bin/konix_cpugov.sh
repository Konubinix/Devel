#!/bin/bash -eux

i=0
while true
do
	sudo cpufreq-set -c $i "$@" || exit 0
	i=$((i+1))
done
