#!/bin/bash

COMMIT="${1:-HEAD}"
git show $COMMIT|grep '^commit'|cut -f2 -d' '
