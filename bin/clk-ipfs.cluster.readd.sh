#!/bin/bash -eu

usage () {
    cat<<EOF
$0

--
N:cid to readd
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi


for cid in ${CLK___ARGS}
do
    rmin=1
    rmax=1
    trap "echo 'Adding again ${cid} to make sure it is not lost before quitting' && ipfs-cluster-ctl pin add --rmin '${rmin}' --rmax '${rmax}' '${cid}'" 2
    echo "Removing ${cid} from cluster"
    # prefer redirecting to dev null rather than --no-status to make sure the pin is removed for real
    ipfs-cluster-ctl pin rm "${cid}" > /dev/null || echo "Failed, please read manually ${cid} to avoid loosing it"
    echo "Adding back ${cid} to cluster"
    ipfs-cluster-ctl pin add --rmin "${rmin}" --rmax "${rmax}" "${cid}"
done
