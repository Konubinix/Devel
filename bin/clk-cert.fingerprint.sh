#!/bin/bash

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

openssl x509 -in "$1" -noout -fingerprint