#!/usr/bin/env bash

rsync -az --info=progress2 "${@}"
