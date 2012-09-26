#!/bin/bash
# Once this script is launched, source the gpg-agent file written in stdout
# And then export the following environment variables
#	 export GPG_AGENT_INFO
#	 export SSH_AUTH_SOCK

source "${KONIX_LIB_DIR}/lib_bash.sh"

if ! which gpg-agent > /dev/null
then
	echo "gpg-agent not available, it cannot be launched" >&2
	exit 1
fi
if [ -f "$GPG_INFO_FILE_NAME" ] \
	&& pgrep -u "$LOGNAME" gpg-agent > /dev/null 2>&1
then
	echo "Using current gpg-agent conf from $GPG_INFO_FILE_NAME" >&2
else
	echo "Starting a new gpg-agent" >&2
	rm -f "$GPG_INFO_FILE_NAME"
	GPG_AGENT_STARTING_CMD='gpg-agent --daemon --enable-ssh-support --write-env-file "$GPG_INFO_FILE_NAME" >/dev/null'
	if ! is_on_linux
	then
		GPG_AGENT_STARTING_CMD="$GPG_AGENT_STARTING_CMD \&"
	fi
	eval $GPG_AGENT_STARTING_CMD
fi
echo "$GPG_INFO_FILE_NAME"
