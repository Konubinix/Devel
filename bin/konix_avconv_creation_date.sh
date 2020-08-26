#!/bin/bash -eu

file="$1"

creation_date="$(avconv -i "${file}" 2>&1 \
	| grep -i creation_time \
	| head -1 \
	| sed -r 's|^.+[Cc][Rr][Ee][Aa][Tt][Ii][Oo][Nn]_[Tt][Ii][Mm][Ee]   : ([0-9][0-9][0-9][0-9]-[0-9]+-[0-9]+)[ T]([0-9]+:[0-9]+:[0-9]+).*$|\1T\2|')"
if avconv -i "${file}" 2>&1 |grep -q -e "handler_name.*:.*Video Handler"
then
    # xp120 => local date
	result="${creation_date}"
elif avconv -i "${file}" 2>&1 |grep -q -e "com.apple.quicktime.make: Apple"
then
	# utc date
	result="${creation_date}+0000"
elif avconv -i "${file}" 2>&1 |grep -q -i 'iCatch Alias Data Handler' # xp120
then
	# local date
	result="${creation_date}"
elif avconv -i "${file}" 2>&1 |grep -q -i 'creation_time.\+Z$'
then
	# utc date
	result="${creation_date}+0000"
else
	# local date
	result="${creation_date}"
fi
echo "${creation_date}"
