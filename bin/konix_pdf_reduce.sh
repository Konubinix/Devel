#!/bin/bash

# taken from
# http://superuser.com/questions/466031/how-do-i-reduce-a-pdfs-size-and-preserve-the-bookmarks
SRC="$1"
[ -n "$SRC" ] || { echo "Must provide the input file name" ; exit 1 ; }
DST="${2:-${SRC%.*}_reduced.${SRC#*.}}"
echo "Reducing $SRC and putting the result in $DST"
gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dPDFSETTINGS=/ebook -sOutputFile="$DST" "$SRC"
