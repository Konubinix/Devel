#!/bin/bash -eu

usage () {
    cat<<EOF
$0

Show all the pin names
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

source _ipfs-cluster.sh

ipfs_cluster_pins
