#!/bin/bash

usage () {
    cat<<EOF
$0 [-r]

Launch a preconfigured gpg-agent. Use -r to restart it.
EOF
}

while getopts ":hr" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        r)
            echo "Restarting"
            pkill -9 gpg-agent
            ;;
    esac
done
shift $((OPTIND-1))

TMP="$(mktemp -d)"
trap "rm -rf '${TMP}'" 0

on_fail () {
    echo "gpg-agent failed:"
    cat "${TMP}/log"
    exit 1
}

run_agent () {
    gpg-agent "$@" > "${TMP}/log" 2>&1
}


if ! run_agent
then
    run_agent \
	    --daemon \
	    --enable-ssh-support \
	    --default-cache-ttl "${KONIX_GPG_CACHE_TTL}" \
	    --default-cache-ttl-ssh "${KONIX_GPG_CACHE_TTL_SSH}" \
	    --max-cache-ttl "${KONIX_MAX_GPG_CACHE_TTL}" \
	    --max-cache-ttl-ssh "${KONIX_MAX_GPG_CACHE_TTL_SSH}" \
        --log-file "${KONIX_GPG_AGENT_LOG_FILE}" \
        --pinentry-program "/bin/pinentry-gtk-2" \
        --debug-level "${KONIX_GPG_AGENT_LOG_LEVEL}"
fi

reload ( ) {
	gpg-connect-agent reloadagent /bye
}
