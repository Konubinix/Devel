#!/bin/bash

emacsclient -e "(progn (require 'org-publish) (org-publish \"public\"))"
