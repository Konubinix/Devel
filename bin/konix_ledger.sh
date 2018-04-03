#!/bin/bash

exec ledger --prepend-format '------\n%(note)\n' -S date "$@"
