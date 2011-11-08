#!/bin/bash
######################################################################
#  \file parent.sh
#
#  \author Samuel Loury (sy3)
#  \date Tue 14:08:33 15/06/2010
######################################################################
function usage(){
	echo "usage:"
	echo "$0 enfant"
}

function assert_commitish(){
	git rev-parse $1 >/dev/null 2>/dev/null
	if [ $? != 0 ]
	then
		echo "'$1' n'est pas une bonne ref"
		usage
		exit 1
	fi
}

function get_name(){
	git name-rev --name-only $1
}

enfant=$1
assert_commitish $enfant

enfant_name=$(get_name $enfant)
assert_commitish $enfant_name

file_to_check=$(git rev-parse --git-dir)/parent/$enfant_name

[ ! -e "$file_to_check" ] && echo "'$file_to_check' n'existe pas" && exit 1

parent_ref=$(cat "$file_to_check")
parent_name=$(get_name $parent_ref)

echo "parent actuel de '$enfant_name' : '$parent_name'"

