#!/bin/bash

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

# taken from
# http://superuser.com/questions/466031/how-do-i-reduce-a-pdfs-size-and-preserve-the-bookmarks
SRC="$1"
[ -n "$SRC" ] || { echo "Must provide the input file name" ; exit 1 ; }
DST="${2:-${SRC%.*}_reduced.${SRC#*.}}"
# QUAL may be:
# - /screen: the lowest resolution and lowest file size but fine for viewing on
# a screen:
# - /ebook: a mid-point in resolution and file size
# - /printer and /prepress: high-quality settings used for printing PDFs.
QUAL="${3:-/ebook}"

RES="${4:-72}"

echo "Reducing $SRC and putting the result in $DST at qual $QUAL with resolution=${RES}"
gs \
	-sDEVICE=pdfwrite \
	-dNOPAUSE \
	-dBATCH \
	-dSAFER \
	-dEmbedAllFonts=true \
	-dSubsetFonts=true \
	-dColorImageDownsampleType=/Bicubic \
	-dColorImageResolution="${RES}" \
	-dGrayImageDownsampleType=/Bicubic \
	-dGrayImageResolution="${RES}" \
	-dMonoImageDownsampleType=/Bicubic \
	-dMonoImageResolution="${RES}" \
	-dCompatibilityLevel=1.3 \
	-dPDFSETTINGS="${QUAL}" \
	-sOutputFile="${DST}" \
	"${SRC}"
