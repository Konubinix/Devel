#!/bin/bash -eu

usage () {
    cat<<EOF
$0

Open an ipfs file
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

cid="$(clk ipfs find@sh ${CLK___ARGS})"
echo "Opening ${cid}"
exec mimeopen "${cid}"
