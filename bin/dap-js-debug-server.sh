#!/bin/sh
exec node ~/.emacs.d/.extension/js-debug/src/dapDebugServer.js "$@" 127.0.0.1
