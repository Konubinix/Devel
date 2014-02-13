#!/bin/bash

is_sourced() {
    ! [ "$BASH_SOURCE" == "$0" ]
}

init_variables() {
    ####################
    ## Initialisation ##
    ####################
    local MY_DIR="$(dirname "${BASH_SOURCE[0]}")"
    local MY_DIR="$(cd "${MY_DIR}" && pwd)"

    ########################
    ## Set env variables. ##
    ########################
    export LD_LIBRARY_PATH="${MY_DIR}/lib:${LD_LIBRARY_PATH}"
    export PATH="${MY_DIR}/bin:${PATH}"
    export EXEC_NAME="$(cat "${MY_DIR}/.execname")"
    export EXEC_PATH="${MY_DIR}/bin/${EXEC_NAME}"
    return 0
}

if ! is_sourced
then
    echo "Launching $BASH_SOURCE is useless. You should run
\$ source $0
instead in order to update your env variable and run the application.
"
    exit 1
fi

# update the env to get the good variables
if init_variables
then
    echo "Environment values updated, you may now want to launch
$EXEC_NAME
"
else
    echo "This must be a broken standalone" >&2
fi
