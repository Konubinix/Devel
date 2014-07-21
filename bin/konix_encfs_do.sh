#!/bin/bash

DIRNAME="${1}"
CRYPTNAME="${2:-${DIRNAME}_crypt}"

DIRNAME="$(readlink -f "${DIRNAME}")"
CRYPTNAME="$(readlink -f "${CRYPTNAME}")"

encfs "${CRYPTNAME}" "${DIRNAME}"
