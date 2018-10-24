#!/bin/bash -x

git annex find --metadata ack=delete | while read line
do
	git annex drop "${line}"
	rm "${line}"
done
