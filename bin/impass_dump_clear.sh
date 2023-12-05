#!/bin/bash

IMPASS_DUMP_PASSWORDS=1 impass dump "$@" 2> /dev/null
