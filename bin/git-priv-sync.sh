#!/bin/bash

. "${KONIX_LIB_DIR}/lib_bash.sh"

pinfo "Freezing current repo"
git-freeze.sh

pinfo "Pulling modification from remote repo"
git pull priv master
konix_assert_last_command "Failed to pull from remote repo"

pinfo "Pushing current info to remote repo"
git push priv master
konix_assert_last_command "Failed to push to remote repo"
