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

git_dir=$(git rev-parse --git-dir)

assert_empty $1
assert_empty $2

enfant=$(git rev-parse $2)
parent=$(git rev-parse $1)

enfant_nom=$(get_name $enfant )
parent_nom=$(get_name $parent )

assert_commitish $enfant_nom
assert_commitish $parent_nom

[ ! -e $git_dir/refs/parent ] && mkdir $git_dir/refs/parent
[ ! -e $git_dir/parent ] && mkdir $git_dir/parent
file_to_create=$(git rev-parse --git-dir)/parent/$enfant_nom
{
	[ ! -e "$file_to_create" ] &&
	echo $parent_nom > $file_to_create &&
	echo "'$file_to_create' créé"
} || {
	echo "'$file_to_create' existe déjà, faire './del.sh $enfant' d'abord" &&
	exit 1
}

file_to_create=$git_dir/refs/parent/$enfant_nom
{
	echo $parent > $file_to_create &&
	echo "'$file_to_create' créé"
} || {
	echo "'$file_to_create' pose un problème à la création je voulais juste mettre '$parent' dedans..."
	exit 1
}

bash "$(which kgit-parent.sh)" "$enfant"

