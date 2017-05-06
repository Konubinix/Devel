#!/bin/bash -eu

usage () {
    cat<<EOF
$0 -k key
EOF
}


while getopts "hk:" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        k)
            KEY="$OPTARG"
            ;;
    esac
done
shift $((OPTIND-1))

tl="$(git rev-parse --show-toplevel)"
keydir="$(git annex examinekey --format='${hashdirmixed}\n' "${KEY}")"
echo "${tl}/.git/annex/objects/${keydir}${KEY}/${KEY}"
