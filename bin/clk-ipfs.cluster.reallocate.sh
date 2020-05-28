#!/bin/bash -eu

usage () {
    cat<<EOF
$0

Re add the pins so that they are reallocated
--
O:-m,--max:int:Maximum number to reallocate:1000
O:--filter:["cluster_error", "error", "pin_error", "pin_queued", "pinned", "pinning", "queued", "remote", "unpin_error", "unpin_queued", "unpinned", "unpinning"]: Filter to use:pin_error
F:-w,--wait/--no-wait:Wait for each reallocation to be complete:True
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi


TMPDIR="$(mktemp -d)"
trap "rm -rf '${TMPDIR}'" 0

statusargs=()
if [ "${CLK___FILTER}" != "None" ]
then
    statusargs+=("--filter" "${CLK___FILTER}")
fi

pinargs=()
if [ "${CLK___WAIT}" != "False" ]
then
    pinargs+=("--wait")
fi

# set -x
echo "Fetching the status"
ipfs-cluster-ctl --enc=json pin ls > "${TMPDIR}/listing"
# loop in reverse order so that the first elements might be caught up by the
# automatic recovering
ipfs-cluster-ctl --enc=json status "${statusargs[@]}"|jq -r '.[].cid."/"'|tac|head -${CLK___MAX}|while read cid
do
    read rmin rmax < <(jq -r '.[] | select(.cid."/" | contains("'${cid}'")) | "\(.replication_factor_min) \(.replication_factor_max)"' < "${TMPDIR}/listing")
    trap "echo 'Adding again ${cid} to make sure it is not lost before quitting' && ipfs-cluster-ctl pin add --rmin '${rmin}' --rmax '${rmax}' '${cid}'" 2
    echo "Removing ${cid} from cluster"
    # prefer redirecting to dev null rather than --no-status to make sure the pin is removed for real
    ipfs-cluster-ctl pin rm "${cid}" > /dev/null || echo "Failed, please read manually ${cid} to avoid loosing it"
    echo "Adding back ${cid} to cluster"
    ipfs-cluster-ctl pin add "${pinargs[@]}" --rmin "${rmin}" --rmax "${rmax}" "${cid}"
    trap 2
done
