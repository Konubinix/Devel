#!/bin/bash

rsync -r --size-only --info=progress2 "${@}"
