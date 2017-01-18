#!/bin/bash -eu

SRC="$(readlink -f "${1}")"
PREF="$(konix_relative_path.py "${SRC}" "${SRC}/../..")"
DST="$(readlink -f "${2}")"
DST="${DST}/${PREF}"
mkdir -p "${DST}/new" "${DST}/cur" "${DST}/tmp"
mkdir -p "${SRC}/new" "${SRC}/cur" "${SRC}/tmp"
echo "${SRC} -> ${DST}"
konix_uniform_mails.py "${SRC}" "${DST}"
