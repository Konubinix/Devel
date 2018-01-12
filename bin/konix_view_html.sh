#!/bin/bash -eu

dir=`mktemp -d`
file="${dir}/msg"
cat > "${file}"
KONIX_VIEW_HTML_KEEP="${KONIX_VIEW_HTML_KEEP:-}"
if [ -z "${KONIX_VIEW_HTML_KEEP}" ]
then
	trap "rm -r $dir" EXIT
fi
{
	konix_munpack.py -i "${file}" -o "${dir}" "$@"
	HTMLs="${dir}"/*html
	for html in ${HTMLs}
	do
		mimeopen "${html}"
	done
} >&2
echo "${dir}"
if [ -z "${KONIX_VIEW_HTML_KEEP}" ]
then
   sleep 10
fi
