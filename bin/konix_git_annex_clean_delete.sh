#!/bin/bash -x

git annex find --metadata ack=delete | while read line
do
	git annex drop --force "${line}"
	rm "${line}"
done
