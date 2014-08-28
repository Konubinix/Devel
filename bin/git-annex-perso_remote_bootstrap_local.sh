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

cat <<EOF > type
local
EOF

cat <<EOF > url
file://$LOCATION
EOF

cat <<EOF > prehook
#! /bin/bash

git-annex-perso_remoteprehook_local.sh -l$LOCATION
EOF

cat <<EOF > sync_posthook
#! /bin/bash

git-annex-perso_remoteposthook_local.sh -l$LOCATION
EOF

cat <<EOF > availhook
#! /bin/bash

git-annex-perso_remoteavailhook_local.sh -l$LOCATION
EOF
