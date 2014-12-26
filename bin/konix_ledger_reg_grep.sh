#!/bin/bash

ledger reg --register-format="%S:%b: %d %P %t\n" "$@"
