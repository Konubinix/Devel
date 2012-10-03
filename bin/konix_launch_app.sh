#!/bin/bash

# make sure the new application will be launched in the good environment
source "${HOME}/init_bin/konix_init_lib.sh"
import_env

exec "$@"
