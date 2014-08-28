#!/bin/bash

. "${KONIX_LIB_DIR}/lib_bash.sh"

usage ( ) {
    cat<<EOF
TODO
EOF
}

while getopts "l:" opt; do
    case $opt in
        h)
            usage
            exit 1
            ;;
        l)
            LOCATION="${OPTARG}"
            ;;
    esac
done
konix_assert_var LOCATION
cd $LOCATION
if [ "$(git config annex.direct)" == "true" ]
then
  git annex sync
else
  git annex merge
fi
