#!/bin/bash

source "${KONIX_LIB_DIR}/lib_bash.sh"

konix_assert_var KONIX_REMOTE_CONTEXTS_DIR

remoterc_log ( ) {
    if [ -z "${KONIX_REMOTE_QUIET}" ]
    then
        echo "## $*"
    elif [ "${KONIX_REMOTE_QUIET}" == "err" ]
    then
        echo "## $*" >&2
    fi
}

remoterc_log_info ( ) {
    remoterc_log "${COLOR_FG_GREEN}$*${COLOR_RESET}"
}

remoterc_error ( ) {
    remoterc_log "${COLOR_FG_RED}ERROR: $*${COLOR_RESET}" >&2
}

remoterc_error_n_quit ( ) {
    remoterc_error "$*"
	exit 1
}

remoterc_warn_n_return ( ) {
    remoterc_warn "$*"
	return 1
}

remoterc_warn ( ) {
    remoterc_log "${COLOR_FG_YELLOW}WARNING: $*${COLOR_RESET}" >&2
}

remoterc_launch_script ( ) {
    if [ -z "${KONIX_REMOTE_QUIET}" ]
    then
        bash "${1}"
    elif [ "${KONIX_REMOTE_QUIET}" == "err" ]
    then
        bash "${1}" >&2
    else
        bash "${1}" > /dev/null 2>&1
    fi
}

remoterc_launch_hook ( ) {
    local hook="${1}"
    local url="${2}"
    local message="${3}"
    if [ -f "${hook}" ]
    then
        remoterc_log "Launching hook ($message)"
        remoterc_launch_script "${hook}" "${url}" \
            || remoterc_warn_n_return "Failed to launch the hook (${message})"
    fi
}

remoterc_extract_remote_info_from_dir ( ) {
    local remote_path="$1"
    remote_url="$(cat ${remote_path}/url)"
    remote_url="$(eval echo $(echo ${remote_url}))"
    remote_ssh_port=22
    [ -f "${remote_path}/ssh_port" ] && remote_ssh_port="$(cat ${remote_path}/ssh_port)"
    remote_availhook="${remote_path}/availhook"
    remote_prehook="${remote_path}/prehook"
}

remoterc_extract_remote_info ( ) {
    local context="$1"
    # the following is not local since it is wanted to be given to the caller
    remote_name="$2"
    local res=1
    remote_path="${KONIX_REMOTE_CONTEXTS_DIR}/${remote_name}"
    remoterc_log "Extracting info from ${remote_path}"
    if [ -f "${remote_path}/url" ]
    then
        remoterc_warn "No context information detected in $remote_path"
        remoterc_extract_remote_info_from_dir "${remote_path}"
        res=0
    else
        konix_assert_var context
        remoterc_log "Look for a context information in ${remote_path} suitable with '${context}'"
        local context_dir="${remote_path}/${context}"
        if [ -d "${context_dir}" ]
        then
            remoterc_log "Found context information ${context}"
            remoterc_extract_remote_info_from_dir "${context_dir}"
            res=0
        fi
    fi
    return ${res}
}

remoterc_setup() {
    local host="$1"
    if [ $# -gt 1 ]
    then
        local forced_context="$2"
    else
        local forced_context=""
    fi
    if [ $# -gt 2 ]
    then
        local excluded_context="$3"
    else
        local excluded_context=""
    fi
    remoterc_compute_contexts
    local found=1
    # add a dump value for the first shift to be ok
    set ${contexts}
    while [ -n "${*}" ] && [ "${found}" != "0" ]
    do
        local context="${1}"
        shift
        if [ "${forced_context}" != "" ] \
            && [ "${forced_context}" != "${context}" ]
        then
            echo "Ignoring context ${context}" >&2
            continue
        fi
        if [ "${excluded_context}" != "" ] \
            && echo "${context}"|grep -q "${excluded_context}"
        then
            echo "Excluding context ${context}" >&2
            continue
        fi
        remoterc_extract_remote_info "${context}" "${host}" \
            || continue
        remoterc_launch_hook \
            "${remote_availhook}" \
            "${remote_url}" \
            "availhook for ${context}" \
            || continue
        remoterc_launch_hook \
            "${remote_prehook}" \
            "${remote_url}" \
            "prehook for ${context}" \
            || continue
        found=0
    done
    return $found
}

remoterc_setup_or_quit () {
    remoterc_setup "$@" || remoterc_error_n_quit "Could not find a suitable context"
}
remoterc_example_sshconfig() {
    cat<<EOF
Host <HOSTNAME>
	 ProxyCommand konix_ssh_proxycommand.sh "%h" "%p" "%r"
EOF
}

remoterc_compute_contexts ( ) {
    local contexts_program="$(which konix_contexts.sh)"
    contexts=""
    if [ -n "${contexts_program}" ]
    then
        contexts="$(${contexts_program}) ${contexts}"
    else
        remoterc_error_n_quit \
            "A program named konix_contexts.sh must be available in the PATH"
    fi
    remoterc_log "I am in contexts ${contexts}"
}

remoterc_quiet () {
    # to be used in script using the stdout, make sure stdout is clear
    if [ -z "${KONIX_REMOTE_QUIET}" ]
    then
        export KONIX_REMOTE_QUIET=err
    fi
}

remoterc_nc () {
	if [ "$KONIX_REMOTE_QUIET" != "" ] \
		|| [ "$KONIX_REMOTE_QUIET" == "err" ]
	then
		VERBOSE_CMD=-v
	fi
	nc -w3 $VERBOSE_CMD "$@"
}
