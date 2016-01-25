#!/bin/bash

source "${HOME}/init_bin/konix_init_lib.sh"
import_env
source "${KONIX_LIB_DIR}/lib_bash.sh"
konix_gpg_use

export DISPLAY=:0.0
eval "$@"
