#!/bin/sh
dir=`mktemp -d`
#trap "rm -r $dir" 0
echo "$dir" > "${HOME}/konix_view_html.dir"

tee "${HOME}/konix_view_html.msg" | {
	echo $dir > ~/test
	ERR_FILE="${HOME}/konix_view_html.err"
	OUT_FILE="${HOME}/konix_view_html.out"
	echo "" > "${OUT_FILE}"
	echo "" > "${ERR_FILE}"

	cat "$@" > "$dir"/msg

	if munpack -C "$dir" -t < "$dir"/msg 2>&1 | tee "${OUT_FILE}" | grep -q 'Did not find'
	then
	    # failed to unpack the mail, try to do it manually
		sed -n '/[Hh][Tt][Mm][Ll]/,$p' "$dir"/msg > $dir/part1.html
		rm "$dir"/msg
	else
		echo "Successfully munpacked msg" >> "${OUT_FILE}"
	fi
	if ! ls "$dir" |grep -q part
	then
		echo "nothing found munpacked" >> "${OUT_FILE}"
	fi
	for i in "$dir"/part*; do
		if grep -q -i -e '<html>' -e 'text/html' "$i"; then
			x-www-browser "$i" &
			sleep 3
			exit 0
		else
			echo "No html part in $i" >> "$OUT_FILE"
		fi
	done
}
