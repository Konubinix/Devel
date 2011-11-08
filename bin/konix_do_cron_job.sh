#!/bin/bash

source ${HOME}/init_bin/konix_import_env.sh .
import_env t

export DISPLAY=:0.0
eval "$@"
