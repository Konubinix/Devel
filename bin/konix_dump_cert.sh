#!/bin/bash

konix_show_cert.sh "$@" 2>/dev/null| sed -n '/BEGIN CERTIFICATE/,/END CERTIFICATE/p'
