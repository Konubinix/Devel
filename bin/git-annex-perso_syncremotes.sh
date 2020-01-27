#!/bin/bash

source "git-annex-perso_lib.sh"
set -u

usage ( ) {
    cat<<EOF
$0 -r <REMOTE> [-p <PATH>]

r: remote to sync with
p: path limiting the data transfer sync (default to .), use ... to \$(git-toplevel.sh)
EOF
}

KONIX_GIT_ANNEX_PERSO_SYNC_SKIP_DATA=""
KONIX_GIT_ANNEX_PERSO_SYNC_FORCE=""
LIMIT_PATH=""
while getopts "hsp:r:F" opt; do
    case $opt in
        h)
            usage
            exit 1
            ;;
        p)
            LIMIT_PATH="${OPTARG}"
            ;;
        r)
            REMOTE="${OPTARG}"
            ;;
        s)
            KONIX_GIT_ANNEX_PERSO_SYNC_SKIP_DATA=1
            ;;
        F)
            KONIX_GIT_ANNEX_PERSO_SYNC_FORCE=1
            ;;
    esac
done
TOP_LEVEL="$(git-toplevel.sh)"
if [ -z "${LIMIT_PATH}" ]
then
    LIMIT_PATH="$(readlink -e .)"
fi
if [ "${LIMIT_PATH}" == "..." ]
then
    LIMIT_PATH="${TOP_LEVEL}"
fi

gaps_log "Path limited to ${LIMIT_PATH}"
gaps_log "Syncing limited by remote ${REMOTE}"

pushd "${TOP_LEVEL}"
fail=0
inc_fail () {
    fail=$((fail + 1))
}
gaps_remotes_fix "${REMOTE}"
if [ -d "${GITANNEXSYNC_REMOTES}" ]
then
    gaps_compute_contexts
    gaps_compute_remotes "${REMOTE}"
    sync_remotes=""
    for remote in ${remotes}
    do
        if gaps_remote_here_p "${remote}"
        then
            gaps_log_info "Not syncing with ${remote} since it is here"
            continue
        fi
        gaps_log_info "Preparing $remote for sync"
        if ! gaps_remote_initialized_p "${remote}"
        then
			gaps_warn "Remote ${remote} not initialized, cannot do anything with it"
			continue
        fi
        if ! gaps_remote_considered_available_p "${remote}"
        then

            gaps_warn "Remote ${remote} not considered available. Cannot do anything with it"
			continue
        fi
        if ! gaps_extract_remote_info "${contexts}" "${remote}"
        then
            gaps_warn "Could not find info for remote ${remote} in contexts ${contexts}"
            inc_fail
            continue
        fi

		if [ "${type}" == "git" ] || [ "${type}" == "ssh" ] || [ "${type}" == "local" ]
		then
	        if ! gaps_launch_availhook "${availhook}" "${url}" "${remote_name}"
            then
                gaps_error "Failed to launch the availhook for ${remote_name}"
                continue
            fi
			if ! gaps_launch_prehook "${prehook}" "${url}" "${remote_name}"
            then
                gaps_error "Failed to launch the prehook for ${remote_name}"
                continue
            fi
		fi
        sync_remotes="${sync_remotes} ${remote}"
	done
    if [ -n "${KONIX_GIT_ANNEX_PERSO_SYNC_SKIP_DATA}" ]
    then
        gaps_log "-> avoid transferring data"
        args=""
    else
        args="-C . --content"
    fi
    gaps_log "Limiting to path ${LIMIT_PATH}"
    set -x
    pushd "${LIMIT_PATH}"
    git annex sync ${args} ${sync_remotes} web bittorrent
    popd
    set +x
    for remote in ${sync_remotes}
    do
        if gaps_remote_here_p "${remote}"
        then
            gaps_log_info "Not syncing with ${remote} since it is here"
            continue
        fi
        gaps_log_info "Post hook of $remote"
        if ! gaps_remote_initialized_p "${remote}"
        then
			gaps_warn "Remote ${remote} not initialized, cannot do anything with it"
			continue
        fi
        if ! gaps_remote_considered_available_p "${remote}"
        then

            gaps_warn "Remote ${remote} not considered available. Cannot do anything with it"
			continue
        fi
        if ! gaps_extract_remote_info "${contexts}" "${remote}"
        then
            gaps_warn "Could not find info for remote ${remote} in contexts ${contexts}"
            inc_fail
            continue
        fi
		# Sync posthook must be done even if no data transfer has been made
		# since file may only have been moved around
		gaps_launch_posthook "${sync_posthook}" "${url}" "sync post hook"
		gaps_launch_posthook "${posthook}" "${url}" "post hook"
	done
else
	gaps_log "No remote to sync with"
fi
exit "${fail}"
