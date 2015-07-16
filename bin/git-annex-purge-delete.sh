#!/bin/bash

git annex find --metadata ack=delete \
    | while read file
      do
          rm -v "${file}"
      done
