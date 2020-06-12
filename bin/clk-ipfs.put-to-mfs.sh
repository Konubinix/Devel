#!/bin/bash -eu

usage () {
    cat<<EOF
$0

--
N:search terms
A:location:str:MFS path
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

cid="$(clk ipfs find@sh ${CLK___ARGS})"
ipfs files cp "${cid}" "${CLK___LOCATION}"
