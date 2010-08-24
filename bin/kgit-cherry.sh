#!/bin/bash
######################################################################
#  \file kgit-cherry.sh
#
#  \author Samuel Loury (sy3)
#  \date Thu 15:26:05 08/07/2010
######################################################################
PARENT_COMMIT="$1"
CHILD_COMMIT="$2"

git cherry "$PARENT_COMMIT" "$CHILD_COMMIT" |
sed -n '
/^+ / {
s/+ //
p
}
' |
{
	while read line
	do
		echo "Cherry picking "
		git cherry-pick $line
	done
}