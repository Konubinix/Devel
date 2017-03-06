#!/bin/bash -eux

which pypdfocr 2>&1 > /dev/null || {
    echo "pip install --user pypdfocr" && exit 1
}

which tesseract 2>&1 > /dev/null || {
    echo "sai tesseract-ocr tesseract-ocr-fra tesseract-ocr-eng" && exit 1
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

pypdfocr -l "${lang}" "$@"
