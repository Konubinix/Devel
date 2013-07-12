#!/bin/bash

source "${HOME}/init_bin/konix_init_lib.sh"
import_env
source "${KONIX_LIB_DIR}/lib_bash.sh"
gpg_agent_start_KONIX

export DISPLAY=:0.0
eval "$@"
