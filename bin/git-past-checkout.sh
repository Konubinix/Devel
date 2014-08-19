#!/bin/bash

# do not use git checkout @{date}
# http://blog.endpoint.com/2014/05/git-checkout-at-specific-date.html
set -eu
DATE="$*"

git checkout `git rev-list -1 --before="${DATE}" HEAD`
