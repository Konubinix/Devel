#! /bin/bash

NAME="shell"
screen -S "${NAME}" -x "$@" || screen -S "${NAME}" "$@"
