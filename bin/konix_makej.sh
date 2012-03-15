#!/bin/bash

NB_CPU="$(konix_nb_cpu.py)"
make -j$((1+NB_CPU)) "$@"
