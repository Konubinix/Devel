#!/bin/sh
dir=`mktemp -d`
trap "rm -r $dir" 0

echo $dir > ~/test
ERR_FILE="${HOME}/konix_view_html.err"
echo "" > "${ERR_FILE}"

cat "$@" > "$dir"/msg

if munpack -C "$dir" -t < "$dir"/msg 2>&1 | grep -q 'Did not find'; then
	# failed to unpack the mail, try to do it manually
	sed -n '/[Hh][Tt][Mm][Ll]/,$p' "$dir"/msg > $dir/part1.html
	rm "$dir"/msg
else
	echo "Successfully munpacked msg" >> "${ERR_FILE}"
fi
if ! ls "$dir" |grep -q part
then
	echo "nothing found munpacked" >> "${ERR_FILE}"
fi
for i in "$dir"/part*; do
	if grep -q -i -e '<html>' -e 'text/html' "$i"; then
		x-www-browser "$i" &
		sleep 3
		exit 0
	else
		echo "No html part in $i" >> "$ERR_FILE"
	fi
done
