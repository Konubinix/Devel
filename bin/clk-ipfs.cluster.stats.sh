#!/bin/bash -eu

usage () {
    cat<<EOF
$0

Show some stats about the status
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

TMPDIR="$(mktemp -d)"
trap "rm -rf '${TMPDIR}'" 0

ipfs-cluster-ctl status > "${TMPDIR}/status.json"

for status in \
    "pinned" \
        "pinning" \
        "pin_queued" \
        "queued" \
        "unpin_queued" \
        "unpinned" \
        "unpinning" \
        "pin_error" \
        "cluster_error" \
        "unpin_error" \
        "error" \
        "remote"
do
    echo "${status}: $(cat "${TMPDIR}/status.json"|grep -i "\b${status}\b"|wc -l)"
done
