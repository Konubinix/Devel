#!/bin/bash -eu

usage () {
    cat<<EOF
$0

EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

for i in *pdf
do
    pc="$(exiftool -p '$PageCount' $i)"
    mv "${i}" "${i%*.pdf}-${pc}.pdf"
done
