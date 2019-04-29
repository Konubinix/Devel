#!/bin/bash

has_ocr ( ) {
    local filename="$1"
    if [ -e "${filename%.pdf}_ocr.pdf" ]
    then
        return 0
    else
        return 1
    fi
}

usage () {
    cat<<EOF
$0 -l lang

ocr a file if not already done
EOF
}

lang=fra
while getopts "hl:" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        l)
            lang="$OPTARG"
            ;;
    esac
done
shift $((OPTIND-1))

filename="${1}"

if has_ocr "${filename}"
then
    echo "${filename} already has an ocr version"
else
	echo "Handling ${filename}"
    clk tesseract --language "${lang}" "${filename}"
fi
