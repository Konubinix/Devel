#!/bin/bash

gpg-agent \
	--daemon \
	--enable-ssh-support \
	--default-cache-ttl "${KONIX_GPG_CACHE_TTL}" \
	--default-cache-ttl-ssh "${KONIX_GPG_CACHE_TTL_SSH}" \
	--max-cache-ttl "${KONIX_MAX_GPG_CACHE_TTL}" \
	--max-cache-ttl-ssh "${KONIX_MAX_GPG_CACHE_TTL_SSH}" \
	--write-env-file "$GPG_INFO_FILE_NAME" \
	sleep 2246400000
# sleep for 100 years
