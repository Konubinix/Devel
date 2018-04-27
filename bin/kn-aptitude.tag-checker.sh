#!/bin/bash

if [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

# use this in cron with something like
# 0 1 * * * path_to_here/konix_aptitude_tag_checker.sh

LIST_PACKAGES="$(mktemp)"
trap "rm '${LIST_PACKAGES}'" 0

aptitude search '~i ?not(~M) ?not(?user-tag(.))' > "${LIST_PACKAGES}"
if [ "$(wc -l < "${LIST_PACKAGES}")" == "0" ]
then
    exit 0
else
    echo "Those packages should have a tag" >&2
    echo "################################" >&2
    cat "${LIST_PACKAGES}" >&2
    exit 1
fi
