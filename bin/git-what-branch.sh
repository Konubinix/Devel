#!/bin/bash

git status -b --porcelain | head -1 | sed -r 's/^## (.+)$/\1/'
