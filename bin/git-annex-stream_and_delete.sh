#!/bin/bash

FILE="${1}"
git-annex-stream.sh "${FILE}"
echo "delete ${FILE}"
echo -n "y/n: "
read ans
if [ "${ans}" == "y" ]
then
	echo "deleting '${FILE}'"
	rm "${FILE}"
fi
