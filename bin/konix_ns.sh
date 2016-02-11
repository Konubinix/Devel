#!/bin/bash

netstat -tupln| grep -i "$@"
