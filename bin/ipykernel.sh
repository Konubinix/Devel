#!/bin/bash

source ~/.virtualenvs/maths/bin/activate
ipython kernel --IPKernelApp.connection_file=kernel_custo.json "$@"
