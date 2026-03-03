#!/usr/bin/env bash

BIBTEX="$(mktemp)"
trap "rm '${BIBTEX}'" 0

xapers
