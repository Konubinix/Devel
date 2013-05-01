#!/bin/bash

emacsclient -e "(progn (require 'org-publish) (save-excursion (konix/org-wiki-generate)))"
