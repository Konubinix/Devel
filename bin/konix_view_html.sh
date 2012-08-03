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

	# Adjust the content-id stuffs. If there is an attachment with something
	# like that
	# Content-Type: TYPE; name="FILE"
	# ...
	# Content-ID: <A>
	#
	# CONTENT
	#
	# And somewhere in the mail something like cid:A, I must replace it by the
	# associated FILE name
	cat "$dir"/msg | _konix_mail_content_id_adjust.py "$dir" > "$dir"/sed

	if munpack -C "$dir" -t < "$dir"/msg 2>&1 | tee "${OUT_FILE}" | grep -q 'Did not find'
	then
	    # failed to unpack the mail, try to do it manually
		sed -n '/[Hh][Tt][Mm][Ll]/,$p' "$dir"/msg > $dir/part1.html
		rm "$dir"/msg
	else
		echo "Successfully munpacked msg" >> "${OUT_FILE}"
		# rename the partN file to get partN.content_type
		sed -n '
/part.\+ (text\// {
   s/\(part.\+\) (text\/\(.\+\))/\1 \2/
   p
}
' < ${OUT_FILE} | while read line
		do
			set $line
			mv "${dir}/$1" "${dir}/$1.$2"
		done
	fi
	if ! ls "$dir" |grep -q part
	then
		echo "nothing found munpacked" >> "${OUT_FILE}"
	fi
	for i in "$dir"/part*.[Hh][Tt][Mm][Ll]; do
        i_new="$i.new.html"
        cat "$i" | bash "$dir"/sed > "${i_new}"
		mimeopen "${i_new}" &
		sleep 3
		exit 0
	done
}
