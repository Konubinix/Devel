#!/bin/bash

# The personal raw info is on KONIX_PERSO_RAW_DIR, the local one is on
# KONIX_PERSO_DIR, get the password from the file KONIX_PERSO_PASS if it exists

# - go to the perso dir
# - perso dir is freezed
# - KONIX_PERSO_RAW_DIR, is encfs mounted in a temp dir
# - the git personal info is pulled pushed to it
# - it is then unmounted
# - go back to working directory

. "${KONIX_LIB_DIR}/lib_bash.sh"
# sanity check
source konix_assert_var.sh "$KONIX_PERSO_RAW_DIR"
source konix_assert.sh "-d '$KONIX_PERSO_RAW_DIR'"

TMP_PERSO_DECRYPT="$(mktemp -d)"
trap "rm -rvf '$TMP_PERSO_DECRYPT'" 0

# init
pushd "$KONIX_PERSO_DIR"
# crypt what need to be
for tobecrypted in *nd
do
	pushd "$tobecrypted"
	konix_gpg_all.py
	popd
done
if [ -n "$KONIX_PERSO_PRE_PUSH_HOOK" ]
then
	eval "$KONIX_PERSO_PRE_PUSH_HOOK"
fi

git-freeze.sh
if [ -f "$KONIX_PERSO_PASS" ]
then
	cat "$KONIX_PERSO_PASS" | encfs -S "$KONIX_PERSO_RAW_DIR" "$TMP_PERSO_DECRYPT"
else
	encfs "$KONIX_PERSO_RAW_DIR" "$TMP_PERSO_DECRYPT"
fi
konix_assert_last_command "Failed to get an enc fs"

# set up a cleaning at exit
trap "fusermount -u '$TMP_PERSO_DECRYPT' && rm -rvf '$TMP_PERSO_DECRYPT'" 0

# sync
git pull "$TMP_PERSO_DECRYPT"
konix_assert_last_command "Failed to pull from the temp dir"
git push "$TMP_PERSO_DECRYPT"
konix_assert_last_command "Failed to push to the temp dir"

# clean
popd
