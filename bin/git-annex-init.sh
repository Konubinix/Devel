#!/bin/bash -x

git annex init ${HOSTNAME}
git config annex.uuid ${HOSTNAME}
