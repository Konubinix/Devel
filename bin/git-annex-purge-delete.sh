#!/bin/bash

git annex find --metadata state=delete \
    | while read file
      do
          rm -v "${file}"
      done
