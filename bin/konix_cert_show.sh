#!/bin/bash

echo| openssl s_client -showcerts -connect "$@"
