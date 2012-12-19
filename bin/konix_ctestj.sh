#!/bin/bash

NB_CPU="$(konix_nb_cpu.py)"
ctest -j$((1+NB_CPU)) "$@"
