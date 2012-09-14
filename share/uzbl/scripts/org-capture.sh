#!/bin/bash

KEY="$1"
HREF="$2"
TITLE="$3"
SELECTION="$4"
emacsclient "org-protocol://capture://$KEY/$HREF/$TITLE/$SELECTION"
