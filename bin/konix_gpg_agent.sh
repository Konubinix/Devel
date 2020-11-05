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

if ! gpg-agent
then
    gpg-agent \
	    --daemon \
	    --enable-ssh-support \
	    --default-cache-ttl "${KONIX_GPG_CACHE_TTL}" \
	    --default-cache-ttl-ssh "${KONIX_GPG_CACHE_TTL_SSH}" \
	    --max-cache-ttl "${KONIX_MAX_GPG_CACHE_TTL}" \
	    --max-cache-ttl-ssh "${KONIX_MAX_GPG_CACHE_TTL_SSH}" \
        --log-file "${KONIX_GPG_AGENT_LOG_FILE}" \
        --debug-level "${KONIX_GPG_AGENT_LOG_LEVEL}"
fi

reload ( ) {
	gpg-connect-agent reloadagent /bye
}

if grep -q '^pinentry-program' "${HOME}/.gnupg/gpg-agent.conf"
then
	if ! grep -q '^pinentry-program /bin/pinentry-gtk-2' "${HOME}/.gnupg/gpg-agent.conf"
	then
		konix_display.py "Replacing current pinentry with pinentry-gtk-2, compatible with tty + X11"
		sed -r -i 's|^pinentry-program.+|pinentry-program /bin/pinentry-gtk-2|' "${HOME}/.gnupg/gpg-agent.conf"
		reload
	fi
else
	konix_display.py "Installing pinentry-gtk-2 as pinentry, compatible with tty + X11"
	echo "pinentry-program /usr/bin/pinentry-gtk-2" >> "${HOME}/.gnupg/gpg-agent.conf"
	reload
fi
