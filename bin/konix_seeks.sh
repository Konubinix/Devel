#!/bin/bash

cd ${KONIX_DEVEL_DIR}/seeks/src
./seeks >> $HOME/seeks.log 2>&1 &
