#!/bin/bash

usage () {
    cat<<EOF
$0 [-p port] [-d dir] [-i ip]

port defaults to ${PORT}
dir defaults to ${DIR}
ip defaults to ${IP}
EOF
}

while getopts "hd:p:i:" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        d)
            DIR="$OPTARG"
            ;;
        p)
            PORT="$OPTARG"
            ;;
        i)
            IP="$OPTARG"
            ;;
    esac
done
shift $((OPTIND-1))
