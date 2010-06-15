#!/bin/bash
######################################################################
#  \file add.sh
#
#  \author Samuel Loury (sy3)
#  \date Tue 12:00:17 15/06/2010
######################################################################
function usage(){
	echo "usage:"
	echo "$0 parent enfant"
}

function assert_empty(){
	[ -z $1 ] && usage && exit 1
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

assert_empty $1
assert_empty $2

enfant=$2
parent=$1

assert_commitish $parent
assert_commitish $enfant

enfant_name=$(get_name $enfant)
assert_commitish $enfant_name

file_to_create=$(git rev-parse --git-dir)/parent/$enfant_name

[ ! -e "$file_to_create" ] &&
git rev-parse $parent > $file_to_create &&
echo "'$file_to_create' créé" ||
echo "'$file_to_create' existe déjà, faire './del.sh $enfant' d'abord" &&
./parent.sh $enfant
