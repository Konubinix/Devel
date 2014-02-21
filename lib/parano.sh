#!/bin/bash

# http://www.davidpashley.com/articles/writing-robust-shell-scripts/
# fail on uninitialized access to variable
set -o nounset # set -u
set -o errexit # set -e
# make pipe command fail
set -o pipefail

pno_trap () {
    trap "$*" INT TERM EXIT
}

pno_untrap () {
    trap - INT TERM EXIT
}
