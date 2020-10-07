#!/bin/bash

source "${KONIX_LIB_DIR}/lib_bash.sh"
source "${KONIX_LIB_DIR}/colors.sh"

GITANNEX_BIG_FILE=".gitannexbig"
GITANNEXSYNC_PRE_HOOK=".gitannexsync_prehook"
GITANNEXSYNC_POST_HOOK=".gitannexsync_posthook"
GITANNEXSYNC_REMOTES=".gitannexremotes"
GITANNEXSYNC_CONFIG=".gitannexconfig"
GITANNEXSYNC_CONTEXTS=".gitannexcontexts"
GITANNEXSYNC_INIT="git-annex-perso_init.sh"
GITANNEXSYNC_WARN_FILE="${HOME}/log/gaps_warn_log"
GITANNEXSYNC_ERROR_FILE="${HOME}/log/gaps_error_log"
GITANNEXPERSO_REMOTECLEAN=""

gaps_log ( ) {
    echo "## $*"
}

gaps_log_info ( ) {
    echo -n "${COLOR_FG_GREEN}"
    gaps_log "$*${COLOR_RESET}"
}

gaps_show_date_maybe ( ) {
    [ -n "${KONIX_GIT_ANNEX_PERSO_SHOW_DATES}" ] && date
}

gaps_compute_remotes_internal ( ) {
    local remotes="$(/bin/ls -1 ${GITANNEXSYNC_REMOTES})"
    all_remotes=""
    local git_remotes="$(git remote)"
    # check that git remotes are ok
    for remote in ${git_remotes}
    do
        if echo ${remotes}|grep -v -q ${remote}
        then
            if git config remote.${remote}.url > /dev/null
            then
                gaps_error "The git remote ${remote} is not known by git-annex-perso-sync"
                if [ -n "${GITANNEXPERSO_REMOTECLEAN}" ] \
                    && gaps_ask_for_confirmation "Remove it?"
                then
                    git remote remove ${remote} && \
                        gaps_log_info "Correctly removed ${remote}" || \
                        gaps_log_error "Failed to remove ${remote}"
                fi
            else
                gaps_log_info "${remote} has no url. It must be a special remote"
            fi
        fi
    done
    # Append to all_remotes only not dead remotes
    for remote in ${remotes}
    do
        local remote_path="${GITANNEXSYNC_REMOTES}/${remote}"
        gaps_extract_context_independent_remote_info "${remote_path}"
        if [ -f "${dead}" ]
        then
            gaps_warn "Remote ${remote} considered dead then ignored"
        else
			if [ "${all_remotes}" == "" ]
			then
				all_remotes="${remote}"
			else
            all_remotes="${all_remotes}
${remote}"
			fi
        fi
    done
}

gaps_compute_remotes ( ) {
    local filter="$1"
    gaps_compute_remotes_internal
    if [ -n "${filter}" ]
    then
        gaps_log "Filter remotes with ${filter}"
	    remotes="$(echo "${all_remotes}"|grep "${filter}")"
    else
        remotes="${all_remotes}"
    fi
    gaps_log "Operations will be limited by remotes:
${remotes}"
}

gaps_error ( ) {
    gaps_log "${COLOR_FG_RED}ERROR: $*${COLOR_RESET}" >&2| {
        if [ -n "${GITANNEXSYNC_ERROR_FILE}" ]
        then
            tee -a "${GITANNEXSYNC_ERROR_FILE}"
        else
            cat
        fi
    } >&2
}

gaps_error_n_quit ( ) {
    gaps_error "$*"
	exit 1
}

gaps_warn ( ) {
    gaps_log "${COLOR_FG_YELLOW}WARNING: $*${COLOR_RESET}" | {
        if [ -n "${GITANNEXSYNC_WARN_FILE}" ]
        then
            tee -a "${GITANNEXSYNC_WARN_FILE}"
        else
            cat
        fi
    } >&2
}

gaps_error_and_quit ( ) {
    gaps_error "$@"
    exit 2
}

gaps_assert ( ) {
    local condition="$1"
    local message="$2"
    eval "${condition}" || {
        gaps_error "${message}"
        gaps_error "exit now"
        exit 2
    }
}

gaps_ask_for_confirmation ( ) {
    local message="${*}"
    echo "${message}"
    echo -n "(y/n): "
	read res
	if [ "$res" == "y" ]
	then
        return 0
	else
        return 1
    fi
}

gaps_source_script_maybe ( ) {
	local script="$1"
	if [ -e "${script}" ]
	then
		gaps_log "Launching ${script}"
		cat "${script}"
		. "${script}"
	fi
}

gaps_launch_script ( ) {
    bash -x "$@"
}

gaps_launch_config ( ) {
    # I don't want to run into disk left space troubles again
    git config annex.diskreserve 200M
	# I don't want to be disturbed by the pinetry during automatic processes
	git config commit.gpgsign false
    if [ -f "${GITANNEXSYNC_CONFIG}" ]
    then
        gaps_log "Launching the config script"
        gaps_launch_script "${GITANNEXSYNC_CONFIG}"
        gaps_assert "[ $? -eq 0 ]" "Failed to launch the config script ${GITANNEXSYNC_CONFIG}"
    fi
}

gaps_launch_availhook ( ) {
    local availhook="${1}"
    local url="${2}"
    local message="${3}"
    gaps_log "Launching the availhook for $message"
    if [ -f "${availhook}" ]
    then
        gaps_launch_script "${availhook}" "${url}" \
            || return 1
    fi
	return 0
}

gaps_launch_prehook ( ) {
    local prehook="${1}"
    local url="${2}"
    local message="${3}"
    gaps_log "Launching the prehook for $message"
    if [ -f "${prehook}" ]
    then
        gaps_launch_script "${prehook}" "${url}" \
            || return 1
    fi
    return 0
}

gaps_launch_freeze_pre_hook ( ) {
    local hook="${1}"
    local message="${2}"
    gaps_log "Launching the freeze prehook for $message"
    if [ -f "${hook}" ]
    then
        gaps_launch_script "${hook}" \
            || gaps_error_and_quit "Failed to launch the freeze prehook for ${message}"
    fi
}

gaps_launch_posthook ( ) {
    local posthook="${1}"
    local url="${2}"
    local message="${3}"
    if [ -f "${posthook}" ]
    then
        gaps_launch_script "${posthook}" "${url}" \
            || gaps_error "Failed to launch the posthook for ${message}"
    fi
}

gaps_remote_initialized_p ( ) {
    local remote_name="${1}"
    git remote | grep -q "^$remote_name$"
}

gaps_remote_here_p ( ) {
    local remote_name="${1}"
    [ "${HOSTNAME}" == "${remote_name}" ]
}

gaps_remote_initialized_or_here_p ( ) {
    local remote_name="${1}"
    gaps_remote_here_p "${remote_name}" \
        || gaps_remote_initialized_p "${remote_name}"
}

gaps_extract_remote_info_from_dir ( ) {
    local remote_path="$1"
	if [ -f "${remote_path}/url" ]
	then
		url="$(cat ${remote_path}/url)"
		url="$(eval echo $(echo $url))"
	else
		url=""
	fi
	if [ -f "${remote_path}/type" ]
	then
		type="$(cat ${remote_path}/type)"
		type="$(eval echo $(echo $type))"
	else
		gaps_warn "Missing type file for ${remote}, assuming ssh"
		type="ssh"
	fi
	readonly="${remote_path}/readonly"
    availhook="${remote_path}/availhook"
    prehook="${remote_path}/prehook"
    posthook="${remote_path}/posthook"
    sync_posthook="${remote_path}/sync_posthook"
}

gaps_extract_context_independent_remote_info ( ) {
    local remote_path="${1}"
    group_file="${remote_path}/group"
	group="$(if [ -e ${group_file} ]; then cat ${group_file} ; fi)"
    wanted_file="${remote_path}/wanted"
	wanted="$(if [ -e ${wanted_file} ] ; then cat ${wanted_file} ; fi)"
	dead="${remote_path}/dead"
}

gaps_remote_update_availability () {
    local remote="$1"
    if ! gaps_remote_initialized_p "${remote}"
    then
        return
    fi
    if [ "$(git config "remote.${remote}.annex-ignore")" == "true" ]
    then
        git config "remote.${remote}.konix-annex-available" false
    else
        git config "remote.${remote}.konix-annex-available" true
    fi
}

gaps_remote_considered_available_p () {
    local remote="$1"
    local res="$(git config "remote.${remote}.konix-annex-available")"
    local git_command_res=$?
    if [ "${git_command_res}" == "0" ]
    then
        [ "${res}" == "true" ]
        return "$?"
    else
        return 0
    fi
}

gaps_remote_disable () {
    local remote="$1"
    if ! gaps_remote_initialized_p "${remote}"
    then
        return
    fi
    gaps_error "stopping syncing with remote ${remote} it and ignoring it"
    git config "remote.${remote}.annex-ignore" true
    git config "remote.${remote}.annex-sync" false
    git config "remote.${remote}.konix-annex-available" false
}

gaps_remotes_fix ( ) {
    local REMOTES="${1}"
    gaps_log_info "## Checking for inconsistencies in remotes"
    if [ -d "${GITANNEXSYNC_REMOTES}" ]
    then
        gaps_compute_contexts
        gaps_compute_remotes "${REMOTES}"
        for remote in ${remotes}
        do
	        gaps_log_info "Checking remote $remote"
            if gaps_remote_here_p "${remote}"
            then
                gaps_log_info "Remote $remote is here"
                continue
            fi

            if gaps_extract_remote_info "${contexts}" "${remote}"
            then
                if gaps_remote_initialized_p "${remote}"
                then
                    # only need to make sure the availability is ok
                    gaps_remote_update_availability "${remote}"
                else
                    # only need to initialize it
                    gaps_warn "Remote $remote_name not initialized"
				    gaps_log "Is it available ?"
				    if ! gaps_launch_availhook "${availhook}" "${url}" "${remote_name}"
					then
						gaps_warn "${remote_name} not available"
						continue
					fi
				    gaps_log "It is available! Now, trying to initialize it"
                    if ! git-annex-perso_initremotes.sh "\b${remote}\b"
                    then
                        gaps_warn "Could not initialize remote ${remote}"
                        if gaps_ask_for_confirmation "Consider it dead?"
                        then
                            local remote_path="${GITANNEXSYNC_REMOTES}/${remote}"
                            gaps_extract_context_independent_remote_info "${remote_path}"
                            echo 1 > "${dead}"
                        fi
                        continue
                    fi
                fi
            else
                gaps_warn "Could not find info for remote ${remote} in contexts ${contexts}"
                if gaps_remote_considered_available_p "${remote}"
                then
                    gaps_remote_disable "${remote}"
                fi
                continue
            fi
            # now, the remote must be initialized and not be here

            # it is a distant remote, check that it is available
            gaps_launch_availhook \
                "${availhook}" \
                "${url}" \
                "availhook of ${remote}"
            avail_res="$?"
            gaps_remote_considered_available_p "${remote}"
            local considered_available="$?"
            # changing the state according to what I know now
            if [ "${avail_res}" != "0" ] && [ "${considered_available}" == "0" ]
            then
                gaps_error "${remote} is not available"
                gaps_remote_disable "${remote}"
            elif [ "${avail_res}" == "0" ] && [ "${considered_available}" != "0" ]
            then
                gaps_log_info "${remote} is now available, stop ignoring it"
                git config "remote.${remote}.annex-ignore" false
                git config "remote.${remote}.annex-sync" true
                git config "remote.${remote}.konix-annex-available" true
            elif [ "${avail_res}" != "0" ] && [ "${considered_available}" != "0" ]
            then
                gaps_warn "remote ${remote} still is ignored since still not available"
            fi

            # do I have to go on (check the new state of availability) ?
            if ! gaps_remote_considered_available_p "${remote}"
            then
                gaps_log "continuing to next remote"
                continue
            fi

            gaps_log_info "The remote ${remote} is correctly initialized and available"
            if ! gaps_remote_here_p "${remote}"
            then
                # sanity check that the url is the same in the config only if the
                # remote is not here
                recorded_url="$(git config remote.${remote}.url)"
                if ( [ "${type}" == "ssh" ] || [ "${type}" == "local" ] || [ "${type}" == "git" ] ) && [ "${recorded_url}" != "${url}" ]
                then
                    gaps_warn "URL mismatch"
                    gaps_log "git config: ${recorded_url}"
                    gaps_log "gitannexremotes: ${url}"
                    if gaps_ask_for_confirmation "Record ${url} in git?"
                    then
                        git config remote.${remote}.url ${url}
                    else
					    if ! gaps_ask_for_confirmation "Continue using ${recorded_url}?"
                        then
						    if gaps_ask_for_confirmation "Remove remote ${remote}?"
						    then
							    git remote remove "${remote}"
						    fi
                            gaps_log "Aboooort"
                            continue
                        fi
                    fi
                fi

                # sanity check that the readonly is the same
                recorded_readonly="$(git config remote.${remote}.annex-readonly)"
                # nothing is the same as false
                if [ -z "${recorded_readonly}" ]
                then
                    recorded_readonly=false
                fi
                if [ -f "${readonly}" ]
                then
                    readonly_value=true
                else
                    readonly_value=false
                fi
                if [ "${recorded_readonly}" != "${readonly_value}" ]
                then
                    gaps_warn "readonly mismatch"
                    gaps_log "git config: ${recorded_readonly}"
                    gaps_log "gitannexremotes: ${readonly_value}"
                    if gaps_ask_for_confirmation "Record ${readonly_value} in git config?"
                    then
                        git config "remote.${remote}.annex-readonly" "${readonly_value}"
                    else
                        if [ "${recorded_readonly}" == "true" ]
                        then
					        gaps_ask_for_confirmation "Remove readonly from .gitannexremotes?" \
                                && rm "${readonly}"
                        else
					        gaps_ask_for_confirmation "Add readonly into .gitannexremotes?" \
                                && touch "${readonly}"
                        fi
                    fi
                fi
            fi

            remote_or_here="${remote}"
            if gaps_remote_here_p "${remote}"
            then
                remote_or_here=here
            fi

            if ! [ -f "${readonly}" ]
            then
                # sanity check that the wanted is the same in the config
                recorded_wanted="$(git annex wanted "${remote_or_here}")"
                if [ "${recorded_wanted}" != "${wanted}" ]
                then
                    gaps_warn "Wanted mismatch"
                    gaps_log "git-annex value: ${recorded_wanted}"
                    gaps_log ".gitannexremote: ${wanted}"
                    if gaps_ask_for_confirmation "Record ${wanted} in git-annex?"
                    then
                        git annex wanted "${remote_or_here}" "${wanted}"
                    elif gaps_ask_for_confirmation "Replace .gitannexremote value (${wanted}) by git-annex one (${recorded_wanted})"
                    then
                        echo "${recorded_wanted}" > "${wanted_file}"
                    fi

                fi

                # sanity check that the group is the same in the config
                recorded_group="$(git-annex-whatgroup.sh "${remote_or_here}")"
                res="$?"
                if [ "$res" == "0" ] && [ "${recorded_group}" != "${group}" ]
                then
                    gaps_warn "group mismatch"
                    gaps_log "git-annex value: ${recorded_group}"
                    gaps_log ".gitannexremote: ${group}"
                    if gaps_ask_for_confirmation "Record ${group} in git-annex?"
                    then
						if [ -n "${recorded_group}" ]
						then
							git annex ungroup "${remote_or_here}" "${recorded_group}"
						fi
                        git annex group "${remote_or_here}" "${group}"
                    elif gaps_ask_for_confirmation "Replace .gitannexremote value (${group}) by git-annex one (${recorded_group})"
                    then
                        echo "${recorded_group}" > "${group_file}"
                    fi
                fi
            fi
        done
    fi
}

gaps_setup_context_independent_remote_info ( ) {
    if [ -n "$wanted" ]
    then
        gaps_log "$remote_name wants $wanted"
        git annex wanted "$remote_name" $wanted
    fi
    if [ -n "$group" ]
    then
        gaps_log "$remote_name is in group $group"
		old_group="$(git annex group "${remote_name}")"
		if [ -n "${old_group}" ]
		then
			git annex ungroup "${remote_name}" "${old_group}"
		fi
        git annex group "${remote_name}" "${group}"
    fi
}

gaps_extract_remote_info ( ) {
    local contexts="$1"
    local res=1
    remote_name="$2"
    remote_path="${GITANNEXSYNC_REMOTES}/${remote_name}"
    gaps_log "Extracting info from ${remote_path}"
    if [ -f "${remote_path}/type" ]
    then
        gaps_log "No context information detected in $remote_path"
        gaps_extract_remote_info_from_dir "${remote_path}"
        res=0
    else
        konix_assert_var contexts
        gaps_log "Look for a context information in ${remote_path} suitable with '${contexts}'"
        for context in ${contexts}
        do
            local context_dir="${remote_path}/${context}"
            if [ -d "${context_dir}" ]
            then
                gaps_log "Found context information ${context}"
                gaps_extract_remote_info_from_dir "${context_dir}"
                res=0
                break
            fi
        done
    fi
    gaps_extract_context_independent_remote_info "${remote_path}"
    return ${res}
}

gaps_compute_contexts ( ) {
    local contexts_program="$(which konix_contexts.sh)"
    contexts=""
    if [ -n "${contexts_program}" ]
    then
        contexts="$(${contexts_program}) ${contexts}"
    fi

    if [ -f "${GITANNEXSYNC_CONTEXTS}" ]
    then
        contexts="$(bash "${GITANNEXSYNC_CONTEXTS}") ${contexts}"
    fi

    gaps_log "I am in contexts ${contexts}"
}

gaps_group () {
    local remote_name="$1"
    local uuid="$(git config remote.${remote_name}.annex-uuid)" \
        | gaps_error_and_quit "Could not get the annex uuid of remote ${remote_name}"
    git show git-annex:group.log|grep "^${uuid}"|cut -d $' ' -f 2
}
