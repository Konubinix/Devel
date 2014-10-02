#!/bin/bash

if [ -n "$TERM" ] \
    && [ -x "/usr/bin/tput" ] \
    && tput colors 2>/dev/null >&2 \
    && [ "`tput colors`" -ge "8" ]
then
    export COLOR_RESET=`tput sgr0`
    export COLOR_BOLD=`tput bold`
    export COLOR_UL=`tput smul`

    export COLOR_FG_BLACK=`tput setaf 0`
    export COLOR_FG_RED=`tput setaf 1`
    export COLOR_FG_GREEN=`tput setaf 2`
    export COLOR_FG_YELLOW=`tput setaf 3`
    export COLOR_FG_BLUE=`tput setaf 4`
    export COLOR_FG_MAGENTA=`tput setaf 5`
    export COLOR_FG_CYAN=`tput setaf 6`
    export COLOR_FG_WHITE=`tput setaf 7`
    export COLOR_BG_BLACK=`tput setab 0`
    export COLOR_BG_RED=`tput setab 1`
    export COLOR_BG_GREEN=`tput setab 2`
    export COLOR_BG_YELLOW=`tput setab 3`
    export COLOR_BG_BLUE=`tput setab 4`
    export COLOR_BG_MAGENTA=`tput setab 5`
    export COLOR_BG_CYAN=`tput setab 6`
    export COLOR_BG_WHITE=`tput setab 7`
else
    export COLOR_RESET=""
    export COLOR_BOLD=""
    export COLOR_UL=""
    export COLOR_FG_BLACK=""
    export COLOR_FG_RED=""
    export COLOR_FG_GREEN=""
    export COLOR_FG_YELLOW=""
    export COLOR_FG_BLUE=""
    export COLOR_FG_MAGENTA=""
    export COLOR_FG_CYAN=""
    export COLOR_FG_WHITE=""
    export COLOR_BG_BLACK=""
    export COLOR_BG_RED=""
    export COLOR_BG_GREEN=""
    export COLOR_BG_YELLOW=""
    export COLOR_BG_BLUE=""
    export COLOR_BG_MAGENTA=""
    export COLOR_BG_CYAN=""
    export COLOR_BG_WHITE=""
fi
export COLOR_BLACK=0
export COLOR_BLUE=4
export COLOR_GREEN=2
export COLOR_CYAN=6
export COLOR_RED=1
export COLOR_PURPLE=5
export COLOR_BROWN=3
export COLOR_LIGHT_GRAY=7
export COLOR_DARK_GRAY=8
export COLOR_LIGHT_BLUE=12
export COLOR_LIGHT_GREEN=10
export COLOR_LIGHT_CYAN=14
export COLOR_LIGHT_RED=9
export COLOR_LIGHT_PURPLE=13
export COLOR_YELLOW=11
export COLOR_WHITE=15

in_color () {
    local color="${1}"
    shift
    echo "${color}$*${COLOR_RESET}"
}
