#!/bin/bash

ID="${1}"
dir=`mktemp -d`
file="${dir}/msg"

notmuch show --format=raw "${ID}" > "${file}"
trap "rm -r $dir" EXIT
konix_munpack.py -i "${file}" -o "${dir}"
suffix=""
if [ "$(ls "${dir}"|grep .html|wc -l)" = "1" ]
then
    html_index="$(ls "${dir}"|grep .html)"
    mv "${dir}/${html_index}" "${dir}/content.html"
    suffix="/content.html"
fi

url="${KONIX_IPFS_GATEWAY}$(ipfa "${dir}"|sed -r 's/^(.+)\?.+/\1/')${suffix}"
echo -n "${url}" | konix_xclip_in_all.sh
echo "${url}"
