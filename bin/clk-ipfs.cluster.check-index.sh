#!/bin/bash -eu

source _ipfs.sh

usage () {
    cat<<EOF
$0

Make sure all the index is in the cluster and vice versa.
--
F:--clean-cluster:Remove the extra files from the cluster
F:--add-to-cluster:Add the extra files to the cluster
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

TMPDIR="$(mktemp -d)"
trap "rm -rf '${TMPDIR}'" 0

cat "${IPFS_INDEX}" | cut -f 1 -d ' ' | cut -f 3 -d / | sort > "${TMPDIR}/index_cids"
ipfs-cluster-ctl pin ls | cut -f 1 -d ' ' | sort > "${TMPDIR}/cluster_cids"
konix_diff_new_lines.sh "${TMPDIR}/cluster_cids" "${TMPDIR}/index_cids" > "${TMPDIR}/index_no_cluster"
konix_diff_new_lines.sh "${TMPDIR}/index_cids" "${TMPDIR}/cluster_cids" > "${TMPDIR}/cluster_no_index"

if [ -s "${TMPDIR}/index_no_cluster" ]
then
    echo "# CIDs in the index but not in the cluster"
    echo
    cat "${TMPDIR}/index_no_cluster"|while read cid
    do
        grep "^/ipfs/${cid}" "${IPFS_INDEX}"
    done
    if [ "${CLK___ADD_TO_CLUSTER}" ]
    then
        cat "${TMPDIR}/index_no_cluster"|while read cid
        do
            ipfs-cluster-ctl pin add -r 2 "${cid}"
        done
    fi

    if [ -s "${TMPDIR}/cluster_no_index" ]
    then
        echo
    fi
fi

if [ -s "${TMPDIR}/cluster_no_index" ]
then
    echo "# CIDs in the cluster but not in the index"
    echo
    cat "${TMPDIR}/cluster_no_index"|while read cid
    do
        echo -n "/ipfs/"
        ipfs-cluster-ctl pin ls "${cid}"
    done
    if [ "${CLK___CLEAN_CLUSTER}" == "True" ]
    then
        cat "${TMPDIR}/cluster_no_index"|while read cid
        do
            echo "Removing ${cid}"
            ipfs-cluster-ctl pin rm --no-status "${cid}"
        done
    fi
fi
