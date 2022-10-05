#!/bin/bash

ID="${1}"
dir=`mktemp -d`
file="${dir}/msg"

notmuch show --format=raw "${ID}" > "${file}"
trap "rm -r $dir" EXIT
clk mail unpack-rfc822 "${file}" --output "${dir}"
suffix=""
if [ "$(ls "${dir}"|grep .html|wc -l)" = "1" ]
then
    suffix="/$(ls "${dir}"|grep .html)"
fi

url="${KONIX_IPFS_GATEWAY}$(ipfa "${dir}"|sed -r 's/^(.+)\?.+/\1/')${suffix}"
if [ -n "$DISPLAY" ]
then
    echo -n "${url}" | konix_xclip_in_all.sh
fi
echo "${url}"
