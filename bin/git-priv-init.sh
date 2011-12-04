#!/bin/bash

. "${LIB_DIR}/lib_bash.sh"
. bash_optparse << EOF
DESCRIPTION_BEGIN
  A simple script that init the private repo
DESCRIPTION_END

ARGUMENTS_BEGIN

# VARIABLE   IS         ARG
# NAME       MANDATORY  NAME

  remote_repo  TRUE      "Remote repository"

ARGUMENTS_END

EOF

pinfo () {
	echo "$*" >&2
}

# ####################################################################################################
# Create the parent dir if it does not exists
# ####################################################################################################
remote_repo_parent="$(dirname "$remote_repo")"
if [ ! -d "$remote_repo_parent" ]
then
	pinfo "$remote_repo_parent does not exist, creating it"
	mkdir -p "$remote_repo_parent"
	konix_assert_last_command "$remote_repo_parent must be a valid directory name"
fi
# ####################################################################################################
# Create the new repo if it does not exists
# ####################################################################################################
if [ ! -d "$remote_repo" ]
then
	pinfo "Cloning current repo to bare $remote_repo"
	git clone --bare ./ "$remote_repo"
	konix_assert_last_command "Failed to clone"
fi
# ####################################################################################################
# If there is already a remote branch priv, replace it with current
# ELSE, create a new remote
# ####################################################################################################
if git remote | grep -q "priv"
then
	pinfo "there is already a priv remote, replace its head with $remote_repo"
	git remote set-url priv "$remote_repo"
	konix_assert_last_command "Failed to reseting remote url"
else
	pinfo "Creating a new remote for $remote_repo with name priv"
	git remote add priv "$remote_repo"
fi
