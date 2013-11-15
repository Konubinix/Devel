#!/bin/bash

# Inspired from https://github.com/svend/home-bin/blob/master/spell
# Check spelling of arguments

usage () {
	cat<<EOF
$0 [-h] [-a aspell_args] [-l lang] words
-h show this help and exits
-a aspell additional arguments
-l lang
EOF
}
ASPELL_ARGS=""
LANG_ARG=""
while getopts "ha:l:" opt; do
	case $opt in
		h)
			usage
			exit 0
			;;
		a)
			ASPELL_ARGS="$OPTARG"
			;;
		l)
			LANG_ARG="-l $OPTARG"
			;;
	esac
done
shift $((OPTIND-1))
echo "$@" | aspell -a $LANG_ARG $ASPELL_ARGS
