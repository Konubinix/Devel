#!/bin/bash

# Keep the oldest time stamp
CURRENT="$1"
OTHER="$2"
if echo $(cat "$CURRENT") - $(cat "$OTHER") | bc -l | grep -q '^-'
then
    # current is older than other, don't touch it
    echo "Keep current stamp since it is older ($(cat "$CURRENT") < $(cat "$OTHER"))"
else
    # other is older, put it in current
    echo "Keep other stamp since it is older ($(cat "$CURRENT") >= $(cat "$OTHER"))"
    cp "$OTHER" "$CURRENT"
fi
