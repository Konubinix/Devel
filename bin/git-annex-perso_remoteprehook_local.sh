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

cd "$LOCATION"
git-bare-fixup.sh
git-annex-perso_freeze.sh
