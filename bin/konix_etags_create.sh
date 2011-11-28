#!/bin/bash
usage () {
	cat<<EOF
$0 [-h] [-d TAGDIR_FILE] [-i TAGINCLUDE_FILE] [-o TAGFILE]

Generate a new tag file for emacs with name TAGFILE that recursively parses the
	directories found in TAGDIR_FILE and add the directories found in
	TAGINCLUDE_FILE as includes

-d : Set TAGDIR_FILE, defaults to ./TAGS_DIRS
-o : Set output TAGFILE, defaults to ./TAGS
-i : Set TAGINCLUDE_FILE, defaults to ./TAGS_INCLUDES
-h : Display this help and exits

TAGDIR_FILE and TAGINCLUDE_FILE format :
They contain newline separated paths
For instance, if the content is
A
B
Then the directories A and B will be took into account
EOF
}

TAGS_FILE="./TAGS"
TAGDIRS_FILE="./TAGS_DIRS"
TAGINCLUDES_FILE="./TAGS_INCLUDES"
APPEND_CMD=""
VERBOSE_CMD=""
while getopts "hi:d:o:av" opt; do
	case $opt in
		h)
			usage
			exit 0
			;;
		i)
			TAGINCLUDES_FILE="$OPTARG"
			;;
		d)
			TAGDIRS_FILE="$OPTARG"
			;;
		o)
			TAGS_FILE="$OPTARG"
			;;
		a)
			APPEND_CMD="-a"
			;;
		v)
			VERBOSE_CMD="--verbose=yes"
	esac
done

# ####################################################################################################
# INCLUDES
# ####################################################################################################
source konix_assert_var.sh "$CONFIG_DIR"
source konix_assert_var.sh "$LIB_DIR"

source "${LIB_DIR}/lib_bash.sh"

# ####################################################################################################
# If a tag files or tag include files does not exists, empty it
# ####################################################################################################
konix_var_points_to_file_or_null_p TAGDIRS_FILE
konix_var_points_to_file_or_null_p TAGINCLUDES_FILE
if [ -z "$TAGDIRS_FILE" -a -z "$TAGINCLUDES_FILE" ]
then
	echo "TAGDIRS_FILE and TAGINCLUDES_FILE are empty, adding a new TAGS_DIR
with . in it and use it" >&2
	konix_etags_add.py -d "."
	TAGDIRS_FILE="./TAGS_DIRS"
fi
if [ -n "$TAGDIRS_FILE" ]
then
	konix_file_to_lines "$TAGDIRS_FILE" " "
	RES="${RES//\\//}"
	TAGDIRS_CMD="-R $RES"
fi
if [ -n "$TAGINCLUDES_FILE" ]
then
	konix_file_to_lines "$TAGINCLUDES_FILE" " --etags-include="
	RES="${RES//\\//}"
	TAGINCLUDES_CMD="--etags-include=$RES"
fi

eval ctags $VERBOSE_CMD \
	--options="'$CONFIG_DIR/ctags'" \
	-e \
	"$APPEND_CMD" \
	-f "'$TAGS_FILE'" \
	$TAGDIRS_CMD \
	$TAGINCLUDES_CMD
echo updating tags with kinds
konix_etags_add_kinds.sh 'TAGS'
if [ $? == 0 ]
then
	echo kinds added
else
	echo failed to add kinds
fi
