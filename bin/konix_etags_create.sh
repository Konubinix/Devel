#!/bin/bash
die () {
    echo $*
    exit 1
}

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
source konix_assert_var.sh "$KONIX_CONFIG_DIR"
source konix_assert_var.sh "$KONIX_LIB_DIR"

konix_var_points_to_file_or_null_p () {
    # ####################################################################################################
    # This function checks if the file whose name is stored in variable whose name
    # is in $1 exists, if not so, it replace the content of the variable by ""
    # ####################################################################################################
    local TAGDIR_VAR="$1"
    eval local TAGDIR_FILE="\$$TAGDIR_VAR"
    if [ ! -f "$TAGDIR_FILE" ]
    then
        echo "$TAGDIR_VAR=$TAGDIR_FILE does not point to existing file, empty it" >&2
        eval "$TAGDIR_VAR"=""
    fi
}

konix_file_to_lines () {
    # ####################################################################################################
    # Param 1 : file
    # Param 2 : sep
    # Param 3 : comment
    #
    # Reads the content of the file, split its lines and put in the RES global
    # variable something of the form "line1"$SEP"line2"$SEP... lines beginning with
    # comment will be avoided
    # ####################################################################################################
    local FILE="$1"
    local SEP="$2"
    local comment="$3"
    source konix_assert_var.sh "$FILE"
    source konix_assert_var.sh "$SEP"
    source konix_assert.sh "-f '$FILE'"
    RES=""
    IFS=$'\n'
    for LINE in $(<$FILE)
    do
        if [ -n "$comment" ] && [ "${LINE#${comment}}" != "${LINE}" ]
        then
            continue
        fi
        if [ -z "$RES" ]
        then
            RES="\"$LINE\""
        else
            RES="$RES$SEP\"$LINE\""
        fi
    done
}


# ####################################################################################################
# If a tag files or tag include files does not exists, empty it
# ####################################################################################################
konix_var_points_to_file_or_null_p TAGDIRS_FILE
konix_var_points_to_file_or_null_p TAGINCLUDES_FILE
if [ -z "$TAGDIRS_FILE" -a -z "$TAGINCLUDES_FILE" ]
then
	echo "TAGDIRS_FILE and TAGINCLUDES_FILE are empty, adding a new TAGS_DIR with . in it and use it" >&2
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
	konix_file_to_lines "$TAGINCLUDES_FILE" " --etags-include=" "#"
	RES="${RES//\\//}"
    if [ -n "$RES" ]
    then
	    TAGINCLUDES_CMD="--etags-include=$RES"
    fi
fi

eval ctags $VERBOSE_CMD \
	 --options="'$KONIX_DEFAULT_CTAGS_CONFIG'" \
	 $( [ -f "${KONIX_CUSTOM_CTAGS_CONFIG}" ] && echo "--options='$KONIX_CUSTOM_CTAGS_CONFIG'" ; ) \
	 -e \
	 "$APPEND_CMD" \
	 -f "'$TAGS_FILE'" \
	 $TAGDIRS_CMD \
	 $TAGINCLUDES_CMD || die "Failed to generate tags"
echo updating tags with kinds
konix_etags_add_kinds.sh 'TAGS'
if [ "$?" == "0" ]
then
	echo kinds added
else
	echo failed to add kinds
fi
