#!/bin/bash

set -ue

REMOTE="$1"
FILE_PATH="$2"
if [ -d "${FILE_PATH}" ]
then
    DIRECTORY="1"
else
    DIRECTORY=""
fi

TOP="$(git-toplevel.sh)"
relative_path="$(python -c "import os.path; print os.path.relpath('${FILE_PATH}', '${TOP}')")"
if [ -n "${DIRECTORY}" ]
then
    relative_path="${relative_path%%/}/*"
fi

wanted="$(git annex wanted "${REMOTE}")"
if [ -n "${wanted}" ] && ( echo "$wanted" | grep -q 'include=' || [ "${wanted}" != "standard" ] )
then
    new_wanted="include=${relative_path} or ( ${wanted} )"
else
    new_wanted="include=${relative_path}"
fi
echo "New wanted = ${new_wanted}"
git annex wanted "${REMOTE}" "${new_wanted}"
