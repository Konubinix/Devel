#!/bin/bash

emacsclient --eval "(w3m-browse-url \"$1\")"
