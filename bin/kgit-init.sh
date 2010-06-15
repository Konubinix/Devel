#!/bin/bash
######################################################################
#  \file kgit-init.sh
#
#  \author Konubinix  (konubinix@gmail.com)
#  \date mar. 23:47:27 15/06/2010
######################################################################
git_dir=$(git rev-parse --git-dir)

if [ -e $git_dir/kgit_inited ]
then
	echo "Déjà initialisé"
	exit 1
fi

function update_file(){
	if [ ! -e "$*" ]
	then
		echo "#!/bin/sh" > "$*"
	fi

	echo "kgit-update-refs.sh && echo 'erreur de maj kgit'" >> "$*"

}

update_file $git_dir/hooks/post-commit
update_file $git_dir/hooks/post-rebase

echo "Initialisation OK" &&
echo OK > $git_dir/kgit_inited