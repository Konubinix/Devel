#!/bin/bash

sed -r 's%(/ipfs/|ipfs:/*)%\n/ipfs/%g' \
	| grep -v konixnostore \
	| sed -r 's|^.*(/ipfs/[a-zA-Z0-9]+).*$|\1|' \
	| grep '/ipfs/[a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9]' \
	| sort | uniq
