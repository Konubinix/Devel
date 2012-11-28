#!/bin/bash

git commit -m "squash! $(git log -1 --format='%s' $@)"
