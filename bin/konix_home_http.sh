#!/bin/bash

set -eu

exec fsserve -v -t http "$HOME" 127.0.0.1 -p 9645
