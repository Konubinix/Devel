#!/bin/bash

source "${KONIX_LIB_DIR}/pslib.sh"

function sob_preexec () {
    if ! test -e "${KONIX_SOB_DIR}"
    then
        mkdir -p "${KONIX_SOB_DIR}"
    fi
    if test "$1" != "byobu"
    then
        echo "$1" > "${KONIX_SOB_DIR}/$$"
    fi
}

function sob_postexec () {
    if test -e "${KONIX_SOB_DIR}"
    then
        rm -rf "${KONIX_SOB_DIR}/$$"
    fi
}

function sob_bash_sessions () {
    ls "${KONIX_SOB_DIR}"|while read pid
do
    if test "${pid}" != "$$" # pass when this is the current bash
    then
        if test "$(ps --no-headers -q "${pid}" -o comm)" = "bash"
        then
            echo "${pid}"
        else
            # cleanup
            rm "${KONIX_SOB_DIR}/${pid}"
        fi
    fi
done
}

function sob_get_running_pid () {
    local parent_pid="$1"
    ps --no-headers --ppid "${parent_pid}" -o pid,args --sort=pid | tail -1
}

function sob_ls () {
    for parent in $(sob_bash_sessions)
    do
        local running="$(sob_get_running_pid "${parent}")"
        if test -n "${running}"
        then
            echo ${running}
        fi
    done
}

function sob_select () (
    TMP="$(mktemp -d)"
    trap "rm -rf '${TMP}'" 0

    sob_ls > "${TMP}/results"
    local args=()
    if test -n "$*"
    then
        args+=(--query="$*")
    fi
    fzf "${args[@]}" < "${TMP}/results"
)

function _sob_extract_pid () {
    cut -f1 -d' '
}

function sob_wait () {
    local pid="$(sob_select "$@"|_sob_extract_pid)"
    if test -n "${pid}"
    then
        ps_wait "${pid}"
    fi
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
    local pid="$(sob_select|_sob_extract_pid)"
    kill "$@" "${pid}"
}
