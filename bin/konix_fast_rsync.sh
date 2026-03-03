#!/usr/bin/env bash

rsync -r --size-only --info=progress2 "${@}"
