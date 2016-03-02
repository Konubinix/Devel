#!/bin/bash

usage () {
    cat<<EOF
$0 [-r]

Launch a preconfigured gpg-agent. Use -r to restart it.
EOF
}

while getopts "hr" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        r)
            echo "Restarting"
            pkill gpg-agent
            ;;
    esac
done
shift $((OPTIND-1))

if ! gpg-agent
then
    gpg-agent \
	    --daemon \
	    --enable-ssh-support \
	    --default-cache-ttl "${KONIX_GPG_CACHE_TTL}" \
	    --default-cache-ttl-ssh "${KONIX_GPG_CACHE_TTL_SSH}" \
	    --max-cache-ttl "${KONIX_MAX_GPG_CACHE_TTL}" \
	    --max-cache-ttl-ssh "${KONIX_MAX_GPG_CACHE_TTL_SSH}"
fi
