#!/usr/bin/env bash

sed -r 's%/p/ba%\n/ipfs/ba%g; s%(/ipfs/|ipfs:/*|https://ipfs.konubinix.eu/[^/]+/)%\n/ipfs/%g' \
	| grep -v konixnostore \
	| sed -r 's|^.*(/ipfs/[a-zA-Z0-9]+).*$|\1|' \
	| grep '/ipfs/[a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9]' \
	| sort | uniq
