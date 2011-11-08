#!/bin/bash
######################################################################
#  \file del.sh
#
#  \author Samuel Loury (sy3)
#  \date Tue 12:00:17 15/06/2010
######################################################################
function usage(){
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

function delete_file(){
	file_to_del="$1"
	[ -e "$file_to_del" ] &&
	rm -f "$file_to_del" &&
	echo "'$file_to_del' remové" ||
	echo "'$file_to_del' n'existe pas"
}

enfant=$1
assert_commitish $1
enfant_name=$(get_name $enfant)

delete_file "$(git rev-parse --git-dir)/parent/$enfant_name"
delete_file "$(git rev-parse --git-dir)/refs/parent/$enfant_name"
