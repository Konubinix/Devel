#!/bin/bash

supervisord -c "${KONIX_SUPERVISORDCONF}" "$@"
