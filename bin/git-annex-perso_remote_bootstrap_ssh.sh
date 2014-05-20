#!/bin/bash

. "${KONIX_LIB_DIR}/lib_bash.sh"

usage ( ) {
    cat<<EOF
TODO
EOF
}

WHERE="."
PORT_CMD=""
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
            PORT="${OPTARG}"
            PORT_CMD="-p ${PORT}"
            ;;
        l)
            LOCATION="${OPTARG}"
            ;;
    esac
done
konix_assert_var URL
konix_assert_var LOCATION
URL_PORT="$URL"
if [ -n "${PORT}" ]
then
    URL_PORT="${URL_PORT}:${PORT}"
fi

cat <<EOF > type
ssh
EOF

cat <<EOF > url
ssh://${URL_PORT}$LOCATION
EOF

cat <<EOF > prehook
#! /bin/bash

git-annex-perso_remoteprehook_ssh.sh $PORT_CMD -u$URL -l$LOCATION
EOF

cat <<EOF > sync_posthook
#! /bin/bash

git-annex-perso_remoteposthook_ssh.sh $PORT_CMD -u$URL -l$LOCATION
EOF

cat <<EOF > availhook
#! /bin/bash

git-annex-perso_remoteavailhook_ssh.sh $PORT_CMD -u$URL -l$LOCATION
EOF
