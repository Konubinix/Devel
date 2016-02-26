#!/bin/bash

ID="${1}"
notmuch show --format=raw "${ID}" | konix_view_html.sh
