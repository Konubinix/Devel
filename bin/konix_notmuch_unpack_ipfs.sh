#!/bin/bash

ID="${1}"
dir=`mktemp -d`
file="${dir}/msg"

notmuch show --format=raw "${ID}" > "${file}"
trap "rm -r $dir" EXIT
konix_munpack.py -i "${file}" -o "${dir}"
url="${KONIX_IPFS_GATEWAY}$(ipfa "${dir}")"
echo -n "${url}" | xclip -i
echo "${url}"
