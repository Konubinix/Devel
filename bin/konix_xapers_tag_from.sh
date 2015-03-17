#!/bin/bash

BIBTEX="$(mktemp)"
trap "rm '${BIBTEX}'" 0

xapers
