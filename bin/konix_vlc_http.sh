#!/bin/bash

vlc --extraintf=luahttp --http-host 0.0.0.0 --http-port 8080 "$@"
