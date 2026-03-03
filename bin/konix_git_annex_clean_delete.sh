#!/usr/bin/env bash
set -x

git annex find --metadata state=delete | while read line
do
	git annex drop --force "${line}"
	rm "${line}"
done
