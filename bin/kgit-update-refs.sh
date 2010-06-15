#!/bin/bash
######################################################################
#  \author Samuel Loury (sy3)
#  \date Tue 12:00:17 15/06/2010
# Met à jour une arborescence de branches
######################################################################
function usage(){
	echo 'usage:' &&
	echo "$0"
	exit 1
}

function assert_empty(){
	[ -z $1 ] && usage && exit 1
}

function assert_commitish(){
	assert_empty $1
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

for enfant_nom in "$(ls $git_dir/parent/)"
do
	# pour chaque enfant
	parent_nom=$(cat $git_dir/parent/$enfant_nom)
	parent=$(git rev-parse $parent_nom) >/dev/null 2>/dev/null
	if [ $? != 0 ]
	then
		#le parent n'existe plus, on supprime l'entrée
		rm -f $git_dir/parent/$enfant_nom $git_dir/refs/parent/$enfant_nom
	else
		echo $parent > $git_dir/refs/parent/$enfant_nom
	fi
done
