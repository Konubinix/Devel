#!/bin/bash -eu

usage () {
    cat<<EOF
$0

Get the ipfs file
--
O:-o,--output:str:The output file
N:Somerhing to match
F:--link/--no-link:Create a link instead of getting the file:True
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
if [ "${CLK___LINK}" == "True" ]
then
    ln -s "$cid" "${CLK___OUTPUT}"
else
    ipfs get "${args[@]}" "${cid}"
fi
