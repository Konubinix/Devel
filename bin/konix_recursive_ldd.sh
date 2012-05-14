#!/bin/bash

echo "WARNING: this script does not work yet"

parse_ldd () {
	ldd $1|grep -v linux|sed 's/.\+ => \(.\+\) (.\+/\1/'
}
parse_file () {
	while read stuff
	do
		parse_ldd $stuff > $2
	done < $1
}
parse_recurse ()
{
	>result_tmp
	parse_ldd $1 > tmp1
	while [ "$(cat tmp1 | grep -v '^ +$')" != "" ]
	do cat tmp1 >> result_tmp
		parse_file tmp1 tmp2
		mv tmp2 tmp1
	done
	cat result_tmp|sort|uniq > result
	rm result_tmp
}
parse_recurse $1
cat result
