#!/bin/bash -x

dir=`mktemp -d`
file="${dir}/msg"
cat > "${file}"
trap "rm -r $dir" EXIT
konix_munpack.py -i "${file}" -o "${dir}" --rel-path
ls "${dir}"
HTMLs="${dir}"/*html
for html in ${HTMLs}
do
    mimeopen "${html}"
done
