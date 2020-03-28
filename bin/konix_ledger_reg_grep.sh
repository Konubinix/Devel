#!/bin/bash

konix_ledger.sh reg --register-format="%S:%b: %d\n%P\n\t%t\t%T\n" "$@"
