#!/bin/bash
source konix_assert_var.sh "$CONFIG_DIR"
source konix_assert_var.sh "$LIB_DIR"

source "${LIB_DIR}/lib_bash.sh"

usage () {
	cat<<EOF
TODO
EOF
}

TAGS_FILE="./TAGS"
TAGDIRS_FILE="./TAGS_DIR"
TAGINCLUDES_FILE="./TAGS_INCLUDE"
APPEND_CMD=""
VERBOSE_CMD=""
while getopts "hi:d:f:av" opt; do
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
		f)
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
# If a tag files or tag include files does not exists, empty it
# ####################################################################################################
konix_var_points_to_file_or_null_p TAGDIRS_FILE
konix_var_points_to_file_or_null_p TAGINCLUDES_FILE
if [ -z "$TAGDIRS_FILE" -a -z "$TAGINCLUDES_FILE" ]
then
	echo "One of $TAGDIRS_FILE or $TAGINCLUDES_FILE must point to an existing file" >&2
	exit 1
fi
if [ -n "$TAGDIRS_FILE" ]
then
	konix_file_to_lines "$TAGDIRS_FILE" " "
	TAGDIRS_CMD="-R $RES"
fi
if [ -n "$TAGINCLUDES_FILE" ]
then
	konix_file_to_lines "$TAGINCLUDES_FILE" " --etags-include="
	TAGINCLUDES_CMD="--etags-include=$RES"
fi

eval ctags $VERBOSE_CMD \
	--options="$CONFIG_DIR/ctags" \
	-e \
	"$APPEND_CMD" \
	-f "$TAGS_FILE" \
	$TAGDIRS_CMD \
	$TAGINCLUDES_CMD
# echo UPDATING TAGS
# konix_etags_add_kinds.sh '%s'
# echo TAGS UPDATED
