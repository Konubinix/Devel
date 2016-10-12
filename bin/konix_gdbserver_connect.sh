#!/bin/bash

gdb -ex "target remote ${KONIX_GDBSERVER_CONNECT}" "$@"
