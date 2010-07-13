#!/bin/bash
######################################################################
#  \file update.sh
#
#  \author Samuel Loury (sy3)
#  \date Tue 12:00:17 15/06/2010
# Met à jour une arborescence de branches
######################################################################
function usage(){
	[ -n "$PS1" ] &&
	echo 'usage:' &&
	echo "$0 commitish"
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

assert_commitish $1

enfant=$(git rev-parse $1)
enfant_nom=$(git name-rev --name-only $enfant)
assert_commitish $enfant_nom

parent_nom=$(cat $git_dir/parent/$enfant_nom 2>/dev/null)
parent=$(git rev-parse $parent_nom)
assert_commitish $parent

if [[ $parent == $enfant ]]
then
	echo "'$enfant' est son propre parent"
	exit 0
fi

bash "$0" $parent_nom
res=$?
if [ $res == 2 ]
then
	echo "echec du rebase de '$parent_nom'"
	exit 2
fi

echo "git rebase $parent_nom $enfant_nom"
git rebase $parent_nom $enfant_nom
res=$?
if [ $res != 0 ]
then
	# on signale l'echec pour arréter le rebase
	exit 2
else
	# update the ref
	echo $parent > $git_dir/refs/parent/$enfant_nom
fi

kgit-update-refs.sh
