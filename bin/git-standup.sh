#!/bin/bash

function yesterworkday()
{
    if [[ "Mon" == "$(date +%a)" ]]
    then
        echo "last friday"
    else
        echo "yesterday"
    fi
}

git log -w --since="$(yesterworkday)" "$@"
