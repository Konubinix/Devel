#!/bin/bash

ID="${1}"
dir=`mktemp -d`
file="${dir}/msg"

notmuch show --format=raw "${ID}" > "${file}"
trap "rm -r $dir" EXIT
konix_munpack.py -i "${file}" -o "${dir}"
if [ "$(ls "${dir}"|grep .html|wc -l)" = "1" ]
then
    html_index="$(ls "${dir}"|grep .html)"
    mv "${dir}/${html_index}" "${dir}/index.html"
fi
url="${KONIX_IPFS_GATEWAY}$(ipfa "${dir}")"
echo -n "${url}" | xclip -i
echo "${url}"
