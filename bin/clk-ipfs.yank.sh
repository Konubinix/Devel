#!/bin/bash -eu

usage () {
    cat<<EOF
$0

Find a file and yank its path to the clipboard
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

clk ipfs find@sh --dmenu | xclip -in
