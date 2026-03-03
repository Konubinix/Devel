#!/usr/bin/env bash

gdb -ex "target remote ${KONIX_GDBSERVER_CONNECT}" "$@"
