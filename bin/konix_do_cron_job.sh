#!/bin/bash

source ${HOME}/init_bin/konix_init_lib.sh .
import_env t

export DISPLAY=:0.0
eval "$@"
