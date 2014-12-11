#!/bin/bash

wget \
    --convert-links \
    --adjust-extension \
    --reject=*robots.txt \
    --no-iri \
    --restrict-file-names=windows \
    --page-requisites \
    --mirror \
    "$@"
