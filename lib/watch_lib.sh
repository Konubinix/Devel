#!/bin/bash

source "${KONIX_LIB_DIR}/lib_bash.sh"

watch () {
    set -eu
    source "${KONIX_LIB_DIR}/lib_bash.sh"

    konix_assert_var KONIX_LOG_DIR
    konix_assert_var KONIX_LOG_ERR_DIR
    mkdir -p "${KONIX_LOG_DIR}"
    mkdir -p "${KONIX_LOG_ERR_DIR}"

    konix_gtk_zenity.py --help > /dev/null || {
        echo "No way to inform you of program ending"
        exit 1
    }
    if ! [ -d "${KONIX_WATCHER_CTRL_DIR}" ]
    then
        echo "KONIX_WATCHER_CTRL_DIR does not point to a directory" >&2
        exit 1
    fi
    program_name="$(basename "${1}")"
    OUTPUT_FILE="${KONIX_LOG_DIR}/${program_name}"
    ERROR_FILE="${KONIX_LOG_ERR_DIR}/${program_name}"
    local DATE="$(date)"
    cat <<EOF|tee -a "${OUTPUT_FILE}" >> "${ERROR_FILE}"
########################################
${DATE}: Launched '$*'
########################################
EOF
    "$@" >> "${OUTPUT_FILE}" 2>> "${ERROR_FILE}" &
    local pid=$!
    cat <<EOF|tee -a "${OUTPUT_FILE}" >> "${ERROR_FILE}"
PID=$pid
EOF
    printf "$pid $*\n${DATE}\n" > "${KONIX_WATCHER_CTRL_DIR}/${program_name}_${pid}"
    trap "rm '${KONIX_WATCHER_CTRL_DIR}/${program_name}_${pid}'" 0
    set +e
    wait %1
    res=$?
    set -e
    konix_gtk_entry.py \
        --info \
        --text "Command line '${*//&/&amp;}' ended with status $res"
}

watch_compute_watch_listing () {
    WATCH_LISTING="$(_watch_ls)"
}

_watch_ls () {
    res=1
    local lines="$(ls -t1 "${KONIX_WATCHER_CTRL_DIR}")"
    while read line
    do
        res=0
        echo "$(readlink -f "${KONIX_WATCHER_CTRL_DIR}/${line}")"
    done <<< "${lines}"
    return $res
}

watch_ls () {
    local n=0
    watch_compute_watch_listing || { echo "No running process" ; exit 1 ; }
    echo "${WATCH_LISTING}" | while read line
    do
        n="$(( n + 1 ))"
        echo -n "$n: "
        cat "${line}"
        echo "-------"
    done
}

_watch_kill() {
    local numbers="$1"
    if [ "${numbers}" == "a" ] || [ -z "${numbers}" ]
    then
        numbers="$(eval echo {1..$(echo "${WATCH_LISTING}"|wc -l)})"
    fi
    local sig="${2:-TERM}"
    for number in ${numbers}
    do
        local to_kill="$(echo "${WATCH_LISTING}"|head -${number}|tail -1)"
        local pid_to_kill="$(echo "${to_kill}"|sed -r 's/^.+_([0-9]+)$/\1/')"
        konix_assert_var to_kill
        kill -${sig} "${pid_to_kill}"
    done
}

watch_kill () {
    watch_ls
    echo "What numbers to kill ('a' or empty=all)?"
    read numbers
    echo "What signal to send (default=TERM)?"
    read sig
    # do not quote sig since it is an optional argument
    _watch_kill "${numbers}" $sig
}
