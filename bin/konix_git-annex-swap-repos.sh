#!/bin/bash -eu

src="$1"
dst="$2"

mv "${src}/.git/annex" "${dst}/.git/annex"
mv "${src}/.git/config" "${dst}/.git/config"
mv "${src}" "${src}.tmp"
mv "${dst}" "${src}"
mv "${src}.tmp" "${dst}"
