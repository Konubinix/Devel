#!/bin/bash

g++ -fdump-rtl-expand "$@"
egypt *.expand|dot -Gsize=100,100 -Grankdir=LR -Tpdf -o graph.pdf
rm *.expand
