#!/bin/bash

beet --format-item 'beets:library:track;${id}' "$@"
