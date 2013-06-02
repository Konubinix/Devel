#!/bin/bash

# Algorithm: freeze the local copy, then clone only the 40 last commits to a
# temporaty location ($KONIX_PERSO_DIR.tmp), if everything went ok, move the
# current perso dir to $KONIX_PERSO_DIR.XXX.old (XXX being anything, got from
# mktemp) and move the temporary clone to $KONIX_PERSO_DIR

. "${KONIX_LIB_DIR}/lib_bash.sh"
# init
TEMP_LOC="${KONIX_PERSO_DIR}.tmp"
OLD_LOC="$(mktemp --suffix .old "${KONIX_PERSO_DIR}.XXX")"

die ( ) {
	MESSAGE="$*"
	echo "$MESSAGE"
	exit 1
}

message ( ) {
	MESSAGE="$*"
	echo "$MESSAGE"
}

# save current state
message "Freezing the perso dir"
pushd "$KONIX_PERSO_DIR"
git-freeze.sh
popd

# clone it to the temporary location
message "Cloning the 40 last commits to ${TEMP_LOC}"
git clone --depth 40 "file://${KONIX_PERSO_DIR}/" "${TEMP_LOC}" || \
	die "Failed to clone ${KONIX_PERSO_DIR}"

message "Moving the perso dir to ${OLD_LOC}"
mv "${KONIX_PERSO_DIR}" "${OLD_LOC}" || \
	die "Could not move the perso dir to the old location"
message "Moving the ${TEMP_LOC} to perso dir"
mv "${TEMP_LOC}" "${KONIX_PERSO_DIR}" || \
	die "Could not move the temp dir to the perso dir"
message "Make the new repository not shallow to be able to refetch from it"
rm "${KONIX_PERSO_DIR}/.git/shallow"
