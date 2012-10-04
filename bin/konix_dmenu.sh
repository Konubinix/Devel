#!/bin/bash
# This script only calls dmenu and allow successive filtering (like M-* in
# icicles). An entry validated with RET is chosen. An entry validated with S-RET
# (that does not match exactly an input) is used to launch a new dmenu with the
# inputs filtered with that entry

# ######################################################################
# Init variables
# ######################################################################
TEMP_DIR=`mktemp -d`
INPUT="$TEMP_DIR/input"
TMP="$TEMP_DIR/tmp"
# command to launch DMENU
DMENU='cat "$INPUT" | dmenu "$@"'

cat > "$INPUT"
# ######################################################################
# Cleaning config
# ######################################################################
clean ( ) {
    rm -rf "$TEMP_DIR"
}
trap "clean" 0

# ######################################################################
# Functions
# ######################################################################
# $1 is the given output, return 0 if the output match exacty one given input,
# else 0
match_output ( ) {
    output="$1"
    grep -qr "^$output\$" "$INPUT"
}

# ######################################################################
# Code :
#
# Attempt to get the input by successive calls to dmenu. If a dmenu call returns
# exactly an given input, then return it. If it returns something not in the
# possible inputs, return it. If it returns a substring matching inputs, launch
# a new dmenu instance with only that entries.
# ######################################################################
OUTPUT="`eval $DMENU`"
while ! match_output "$OUTPUT"
do
    # this should be a substring of input, filter with it
    grep "$OUTPUT" "$INPUT" > "$TMP"
    mv "$TMP" "$INPUT"
    if [ `cat "$INPUT" | wc -l` == "0" ]
    then
        return "$OUTPUT"
    fi

    OUTPUT="`eval $DMENU`"
    if [ "$?" != "0" ]
    then
        exit 1
    fi
done
echo "$OUTPUT"
