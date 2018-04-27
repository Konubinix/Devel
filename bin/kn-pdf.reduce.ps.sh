#!/bin/bash

if [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

# taken from
# http://superuser.com/questions/466031/how-do-i-reduce-a-pdfs-size-and-preserve-the-bookmarks
SRC="$1"
[ -n "$SRC" ] || { echo "Must provide the input file name" ; exit 1 ; }
DST="${2:-${SRC%.*}_reduced.${SRC#*.}}"
TMP=`mktemp`

echo "Reducing $SRC and putting the result in $DST via conversion in ps"
pdf2ps "${SRC}" "${TMP}"
ps2pdf "${TMP}" "${DST}"
