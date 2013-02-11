#!/bin/bash

ENGINES=`konix_web_search_engines.sh`
ENGINE=`echo "$ENGINES" | konix_dmenu_custom.sh`
echo "$ENGINE"
