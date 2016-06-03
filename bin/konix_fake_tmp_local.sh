#!/bin/bash

if ! [ -e tmp ]
then
    mkdir tmp
fi

exec proot -b tmp:/tmp "$@"
