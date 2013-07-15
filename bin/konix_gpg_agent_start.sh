#!/bin/bash
# Once this script is launched, source the gpg-agent file written in stdout
# And then export the following environment variables
#    export SSH_AGENT_PID
#	 export GPG_AGENT_INFO
#	 export SSH_AUTH_SOCK

source "${KONIX_LIB_DIR}/lib_bash.sh"

if ! which gpg-agent > /dev/null
then
	echo "gpg-agent not available, it cannot be launched" >&2
	exit 1
fi
GPG_AGENT_PIDS=`pgrep -u "$LOGNAME" gpg-agent`
GPG_AGENT_EXISTS="$?"
gpg_agent_zombi_p ( ) {
    for pid in ${GPG_AGENT_PIDS}
    do
        if ! ps "${pid}" | grep -q Zs
        then
            return 1
        fi
    done
    return 0
}

if [ "${GPG_AGENT_EXISTS}" == "0" ] \
   && ! gpg_agent_zombi_p \
   && [ -f "$GPG_INFO_FILE_NAME" ] \

then
	echo "Using current gpg-agent conf from $GPG_INFO_FILE_NAME" >&2
else
	echo "Starting a new gpg-agent" >&2
	rm -f "$GPG_INFO_FILE_NAME"
	GPG_AGENT_STARTING_CMD='gpg-agent
 --daemon
 --enable-ssh-support
 --default-cache-ttl ${KONIX_GPG_CACHE_TTL}
 --default-cache-ttl-ssh ${KONIX_GPG_CACHE_TTL_SSH}
 --write-env-file "$GPG_INFO_FILE_NAME" >/dev/null'
	if ! is_on_linux
	then
		GPG_AGENT_STARTING_CMD="$GPG_AGENT_STARTING_CMD \&"
	fi
	eval $GPG_AGENT_STARTING_CMD
fi
