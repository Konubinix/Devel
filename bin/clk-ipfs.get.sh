#!/bin/bash -eu

usage () {
    cat<<EOF
$0

Get the ipfs file
--
O:-o,--output:str:The output file
N:Somerhing to match
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

args=()
if [ "${CLK___OUTPUT}" != "" ]
then
    args+=("--output" "${CLK___OUTPUT}")
fi
cid="$(clk ipfs find@sh ${CLK___ARGS})"
ipfs get "${args[@]}" "${cid}"
