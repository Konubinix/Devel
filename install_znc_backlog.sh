#!/bin/bash -eux

sudo apt install znc-dev

TMP_DIR="${HOME}/tmp/znc-backlog"
trap "rm -rf '${TMP_DIR}'" 0
git clone https://github.com/FruitieX/znc-backlog "${TMP_DIR}"
pushd "${TMP_DIR}"
make
cp backlog.so "${KONIX_PERSO_DIR}/znc/modules/backlog.so"
popd
