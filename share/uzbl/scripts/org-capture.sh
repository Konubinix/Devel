#!/bin/bash

HREF="$1"
TITLE="$2"
SELECTION="$3"
emacsclient "org-protocol://capture://B/$HREF/$TITLE/$SELECTION"
