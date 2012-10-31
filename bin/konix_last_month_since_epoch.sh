#!/bin/bash

NUMBER_OF_MONTH="${1:-1}"
echo "$(( $(date +'%s') - 3600 * 24 * 30 * $NUMBER_OF_MONTH))"
