#!/bin/bash -eu

source _clk.sh
source _ipfs.sh
source _ipfs-cluster.sh

usage () {
    cat<<EOF
$0

Add a file in ipfs.
--
A:file:str:The file to add
N:The metadata to add
O:--rmin:int:Minimal number of allocations:-1
O:--rmax:int:Maximal number of allocations:-1
O:-r,--replications:int:Number of replications (0 to remove):2
O:--allocs:$(ipfs_cluster_list_peers|list_to_choice):The allocs
F:--wait/--no-wait:Wait for the file to be correctly pinned everywhere:True
F:-v,--verbose:set -x
F:--path-as-metadata/--no-path-as-metadata:Use the file path as metadata:True
F:--delete/--no-delete:Delete the origin file afterwards:True
F:--assert-not-present/--no-assert-not-present:Assert that the element is not already in the index:True
O:-r,--recipient:str:Recipient to use in case of gpg encrypting the content
O:--key:$(ipfs_list_ipns_names|list_to_choice):The name of the ipfs key to update
F:-k,--add-to-clipboard:Add the cid to the clipboard
O:--replace:str:cid to use to replace in the index, instead of using a new one (= to interactively choose, key to replace the existing key hash)
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

FILE="$(konix_absolute_path.py "${CLK___FILE}")"

if [ "${CLK___REPLICATIONS}" != "0" ]
then
    CLK___RMIN="${CLK___REPLICATIONS}"
    CLK___RMAX="${CLK___REPLICATIONS}"
fi
if [ "${CLK___VERBOSE}" == "True" ]
then
    set -x
fi

TMPDIR="$(mktemp -d)"
trap "rm -rf '${TMPDIR}'" 0

if [ "${CLK___RECIPIENT}" != "" ]
then
    FILE_CONTENT="${TMPDIR}/file.gpg"
    gpg --encrypt --recipient "${CLK___RECIPIENT}" --output "${FILE_CONTENT}" "$(readlink -f "${FILE}")"
else
    FILE_CONTENT="$(readlink -f "${FILE}")"
fi

cid="$(ipfs add --recursive --quieter --progress --pin=false "${FILE_CONTENT}")"
path="/ipfs/${cid}"
if grep -q "^${path}" "${IPFS_INDEX}" && [ "${CLK___ASSERT_NOT_PRESENT}" == "True" ]
then
    echo "Stopping because ${path} is already in the index (see the status bellow)"
    ipfs-cluster-ctl status "${cid}"
    exit 1
fi

args=()
if [ "${CLK___ALLOCS}" != "" ]
then
    args+=("--allocs" "${CLK___ALLOCS}")
fi

args+=(--rmin "${CLK___RMIN}" --rmax "${CLK___RMAX}" "${cid}")

echo "Adding ${path} to cluster" >&2
ipfs-cluster-ctl pin add "${args[@]}" >&2

if grep -q "^${path}" "${IPFS_INDEX}"
then
    echo "${path} Already added into the index" >&2
elif [ -n "${CLK___REPLACE}" ]
then
    if [ "${CLK___REPLACE}" == "=" ]
    then
        oldcid="$(clk ipfs find@sh)"
    elif [ "${CLK___REPLACE}" == "key" ]
    then
        keycid="$(ipns_key_name_to_cid "${CLK___KEY}")"
        oldcid="$(ipfs_remove_prefix "$(ipfs resolve "/ipns/${keycid}")")"
    else
        oldcid="${CLK___REPLACE}"
    fi
    echo "${oldcid} -> ${path} in the index"
    clk ipfs update@sh "${oldcid}" "${path}"
    echo "Done"
else
    echo "Adding ${path} to the index" >&2
    metadata="added=$(date -Iseconds) "
    if [ "${CLK___PATH_AS_METADATA}" == "True" ]
    then
        metadata+="${FILE} "
    fi
    if [ "${CLK___RECIPIENT}" != "" ]
    then
        metadata+="gpg encrypted for ${CLK___RECIPIENT}"
    fi
    metadata+="${CLK___ARGS} "
    echo "${path} ${metadata}" >> "${IPFS_INDEX}"
fi

if [ -n "${CLK___ADD_TO_CLIPBOARD}" ]
then
    echo "${path}"|tr -d '\n'|xclip -in
    echo "Added to clipboard" >&2
fi

if [ -n "${CLK___KEY}" ]
then
    remount=""
    if ! test -s "/ipns"
    then
        remount=1
        fusermount -u /ipns
    fi
    ipfs name publish --key "${CLK___KEY}" "${cid}"
    if [ -n "${remount}" ]
    then
        ipfs mount
    fi
fi

if [ "${CLK___WAIT}" == "True" ]
then
    echo "Waiting for ${path} to be stable" >&2
    ipfs-cluster-ctl pin add --wait "${args[@]}" >&2
fi
if [ "${CLK___DELETE}" == "True" ]
then
    echo "Removing ${FILE}" >&2
    rm -rf "${FILE}"
fi

echo "${cid}"
