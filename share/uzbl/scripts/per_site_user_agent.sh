#!/bin/bash

# Reads the file ${UZBL_DATA_DIR}/useragents.txt and tries to match the uri to
# associate a new user agent. The user agent file can be specified with the
# UZBL_PER_SITE_USERAGENT_FILE environment variable. The format of the read file
# is : uri|useragent. Each line contains an association uri|useragent. For
# instance, if I want http://somesite.com to be given useragent A and another
# site containing example.com in its uri to be given useragent B. I would write
# http://somesite.com|A in the first line and example.com|B in the second. The
# file is parsed from top to bottom. The first match decides the user agent. If
# no match, the user agent is not changed.

. "$UZBL_UTIL_DIR/uzbl-dir.sh"
UZBL_URI="$1"

USER_AGENT_FILE="${UZBL_PER_SITE_USERAGENT_FILE:-${UZBL_DATA_DIR}/useragents.txt}"
NEW_USER_AGENT=""
IFS='|'
while read uri useragent
do
    if [[ "$UZBL_URI" =~ "$uri" ]]
    then
        NEW_USER_AGENT="$useragent"
        break
    fi
done < "${USER_AGENT_FILE}"

if [ "$NEW_USER_AGENT" ]
then
    echo "set useragent = ${NEW_USER_AGENT}" > "$UZBL_FIFO"
fi
