#!/bin/bash
dir=`mktemp -d`
trap "rm -r $dir" 0
cat "$@" > "${dir}/msg"
konix_munpack.py -i "${dir}/msg" -o "${dir}"
HTMLs="${dir}"/*html
for html in ${HTMLs}
do
    mimeopen "${html}"
done
