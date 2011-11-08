#!/usr/bin/env bash

sed -i -n -e '
/^.*class .*\(.\+\).*.*\(::\)\?\1/ {	# Class déclaration (C++,Python)
       s/\(.\+\)/\1 CLASS/
}
/^.*#define .*\(.\+\).*\1/ {	# C Preprocessor macro
       s/\(.\+\)/\1 DEFINE/
}
/^.*def .*\(.\+\).*\1/ { # Python function définition
       s/\(.\+\)/\1 DEF/
}
/^.*enum .*\(.\+\).*.*\(::\)\?\1/ { # C enum
       s/\(.\+\)/\1 ENUM/
}
p
' "$1"
