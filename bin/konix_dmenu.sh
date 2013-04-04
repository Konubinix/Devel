#!/bin/bash

# This script only calls dmenu and allow successive filtering (like M-* in
# icicles). An entry validated with RET is chosen. An entry validated with S-RET
# (that does not match exactly an input) is used to launch a new dmenu with the
# inputs filtered with that entry. If the user exits dmenu with ESC, the program
# stops. If the user validates the empty string with S-RET, the filter is
# aborted.

# ######################################################################
# Init variables
# ######################################################################
TEMP_DIR=`mktemp -d`
INPUT="$TEMP_DIR/input"
FIRST_INPUT="$TEMP_DIR/first_input"
TMP="$TEMP_DIR/tmp"
# command to launch DMENU
DMENU='cat "$INPUT" | dmenu "$@"'

cat > "$INPUT"
cp "$INPUT" "$FIRST_INPUT"
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
    grep -qir "^$output\$" "$INPUT"
}

assert_succeeded ( ) {
    if [ "$?" != "0" ]
    then
        exit 1
    fi
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
assert_succeeded
while ! match_output "$OUTPUT"
do
    # put in tmp what will be the next input
    if [ "$OUTPUT" == "" ]
    then
        cp "$FIRST_INPUT" "$TMP"
    else
        # this should be a substring of input, filter with it
        grep -i "$OUTPUT" "$INPUT" > "$TMP"
    fi
    # make the tmp file the new input
    mv "$TMP" "$INPUT"
    # if the resulting input is empty, return the last output
    if [ `cat "$INPUT" | wc -l` == "0" ]
    then
        echo "$OUTPUT"
        exit 0
    fi

    OUTPUT="`eval $DMENU`"
    assert_succeeded
done
echo "$OUTPUT"
