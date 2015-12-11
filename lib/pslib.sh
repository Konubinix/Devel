#!/bin/bash

PS_WAITING_TIME=2 # seconds
PS_LS=ps_ls
PS_SELECT=ps_select
PS_LS_LINE_TO_PID=ps_ls_line_to_pid

function ps_uid () {
    id -u
}

function ps_ls () {
    # I can't directly pipe the result into grep because I don't want grep in
    # the results
    local res_file="$(mktemp)"
    pgrep -u "$(ps_uid)" --list-full '' > "${res_file}"
    grep -i "$*" "${res_file}"
    local res=$?
    rm "${res_file}"
    return $res
}

function ps_ls_line_to_pid () {
    cut -f 1 -d ' '
}

function ps_select () {
    local lines="$("${PS_LS}")"
    local choice
    read -p "$(echo "${lines}" | cat -n)
Which one?" choice
    local chosen_line="$(echo "${lines}"|head -n"${choice}"|tail -n1)"
    local chosen_pid="$(echo ${chosen_line}|"${PS_LS_LINE_TO_PID}")"
    echo "${chosen_pid}"
}

function ps_wait () {
    local chosen_match="$1"
    local chosen_pid
    if [ -n "${chosen_match}" ]
    then
        chosen_pid="$("${PS_LS}"|grep "${chosen_match}"|head -n1|"${PS_LS_LINE_TO_PID}")"
    else
        chosen_pid="$("${PS_SELECT}")"
    fi
    if [ -z "${chosen_pid}" ]
    then
        echo "Could not select a process" >&2
        return 1
    fi
    echo "Waiting for ${chosen_pid} ($(ps --no-headers -p "${chosen_pid}" -o args))"
    while ps -p "${chosen_pid}" > /dev/null
    do
        sleep "${PS_WAITING_TIME}"
    done
}

function ps_xeyes () {
    ps_wait "$@"
    xeyes
}
