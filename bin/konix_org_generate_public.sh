#!/usr/bin/env bash

emacsclient -e "(progn (require 'org-publish) (org-publish \"public\"))"
