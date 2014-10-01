#!/bin/bash

. "${KONIX_LIB_DIR}/lib_bash.sh"

usage ( ) {
    cat<<EOF
TODO
EOF
}
while getopts "u:p:l:" opt; do
    case $opt in
        h)
            usage
            exit 1
            ;;
        u)
            URL="${OPTARG}"
            ;;
        p)
            PORT="-p${OPTARG}"
            ;;
        l)
            LOCATION="${OPTARG}"
            ;;
    esac
done
konix_assert_var URL
konix_assert_var LOCATION
cat <<EOF|ssh -t $PORT "$URL"
cd "$LOCATION"
\${HOME}/init_bin/konix_do_cron_job.sh git-bare-fixup.sh
\${HOME}/init_bin/konix_do_cron_job.sh git-annex-perso_freeze.sh
EOF
