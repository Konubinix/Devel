#!/bin/bash

exec ledger --prepend-format '  %(note)\n' -S date "$@"
