#!/bin/bash -eu

source /home/sam/.virtualenvs/maths36/bin/activate

is_sourced () {
    ! [ "$BASH_SOURCE" == "$0" ]
}

if ! is_sourced
then
    if [ "$#" == "0" ]
    then
        echo "source the script or use it to wrap a command line"
        exit 1
    fi
    exec "$@"
fi
