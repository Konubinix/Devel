#!/bin/bash

source "${KONIX_LIB_DIR}/pslib.sh"

_SOB_SETUP_DEPTH=0

trap _sob_reset EXIT SIGQUIT  SIGKILL SIGHUP SIGINT SIGQUIT SIGILL SIGTRAP SIGABRT SIGTERM

function _sob_setup () {
    if [ "${_SOB_SETUP_DEPTH}" == "0" ]
    then
        _OLD_PS_LS="${PS_LS}"
        _OLD_PS_SELECT="${PS_SELECT}"
        PS_LS=sob_ls
        PS_SELECT=sob_select
        _SOB_SETUP_DEPTH=1
    else
        _SOB_SETUP_DEPTH="$((_SOB_SETUP_DEPTH + 1))"
    fi
}

function _sob_reset () {
    PS_LS="${_OLD_PS_LS}"
    PS_SELECT="${_OLD_PS_SELECT}"
    unset _OLD_PS_LS
    unset _OLD_PS_SELECT
    unset _SOB_SETUP
    _SOB_SETUP_DEPTH=0
}

function _sob_teardown () {
    [ "${_SOB_SETUP_DEPTH}" == "0" ] && return 0
    _SOB_SETUP_DEPTH="$((_SOB_SETUP_DEPTH - 1))"
    if [ "${_SOB_SETUP_DEPTH}" -le 0 ]
    then
        _sob_reset
    fi
}

function sob_uid () {
    ps_uid
}

function sob_sons_of_terminal_emulator () {
    cat "${HOME}/.bash_ppid"
}

function sob_bash_sessions () {
    pgrep -u "$(sob_uid)" --parent "$(sob_sons_of_terminal_emulator)"
}

# implements ps_ls
function sob_ls () {
    local res_file="$(mktemp)"
    for parent in $(sob_bash_sessions)
    do
        pgrep --parent "$parent" -u "$(sob_uid)" --list-full ''
    done > "${res_file}"
    grep -i "$*" "${res_file}"
    local res="$?"
    rm "${res_file}"
    return $res
}

# implements ps_select
function sob_select () {
    _sob_setup
    ps_select "$@"
    _sob_teardown
}

function sob_wait () {
    _sob_setup
    ps_wait "$@"
    _sob_teardown
}

function sob_xeyes () {
    sob_wait "$@"
    xeyes
}

function sob_notify () {
    sob_wait "$@"
    konix_display.py "$*"
}

function sob_kill () {
    _sob_setup
    local pid="$(ps_select)"
    kill "$@" "${pid}"
    _sob_teardown
}
