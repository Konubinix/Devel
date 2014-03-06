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
RES=1
gaps_remotes_fix "${REMOTE}"
if [ -d "${GITANNEXSYNC_REMOTES}" ]
then
    gaps_compute_contexts
    gaps_compute_remotes "${REMOTE}"
	for remote in ${remotes}
    do
		gaps_log_info "Syncing with $remote"
        if ! gaps_extract_remote_info "${contexts}" "${remote}"
        then
            gaps_warn "Could not find info for remote ${remote} in contexts ${contexts}"
            continue
        fi
        if ! gaps_remote_initialized_p "${remote}"
        then
            gaps_warn_and_continue "Remote $remote_name not initialized, cannot do anything with it"
        fi

        gaps_log "Syncing $remote_name with me"
        OLD_MASTER_SHA=`git log -1 master --pretty=format:"%H"`
        OLD_GIT_ANNEX_SHA=`git log -1 git-annex --pretty=format:"%H"`
        OLD_REMOTE_MASTER_SHA=`git log -1 ${remote_name}/synced/master --pretty=format:"%H"`
        OLD_REMOTE_GIT_ANNEX_SHA=`git log -1 ${remote_name}/synced/git-annex --pretty=format:"%H"`
		gaps_log "Here master OLD: ${OLD_MASTER_SHA}"
		gaps_log "Here git-annex OLD: ${OLD_GIT_ANNEX_SHA}"
		gaps_log "${remote_name} synced master OLD: ${OLD_REMOTE_MASTER_SHA}"
		gaps_log "${remote_name} synced git-annex OLD: ${OLD_REMOTE_GIT_ANNEX_SHA}"

        gaps_launch_availhook_or_continue "${availhook}" "${url}" "${remote_name}"
        gaps_launch_prehook "${prehook}" "${url}" "${remote_name}"

        # configure the post hook to be run even if the sync is aborted
        gaps_log "Configuring the post hook for remote ${remote}"
        trap gaps_posthook_launch_maybe 0

        gaps_log "Git annex syncing with ${remote_name}"
        git annex sync "${remote_name}" || \
			gaps_error_n_quit "Failed git annexing with ${remote}"
        NEW_MASTER_SHA=`git log -1 master --pretty=format:"%H"`
        NEW_GIT_ANNEX_SHA=`git log -1 git-annex --pretty=format:"%H"`
        NEW_REMOTE_MASTER_SHA=`git log -1 ${remote_name}/synced/master --pretty=format:"%H"`
        NEW_REMOTE_GIT_ANNEX_SHA=`git log -1 ${remote_name}/synced/git-annex --pretty=format:"%H"`
		gaps_log "Here master NEW: ${NEW_MASTER_SHA}"
		gaps_log "Here git-annex NEW: ${NEW_GIT_ANNEX_SHA}"
		gaps_log "${remote_name} synced master NEW: ${NEW_REMOTE_MASTER_SHA}"
		gaps_log "${remote_name} synced git-annex NEW: ${NEW_REMOTE_GIT_ANNEX_SHA}"
        if [ -n "${KONIX_GIT_ANNEX_PERSO_SYNC_SKIP_DATA}" ]
        then
            gaps_log "-> avoid transferring data"
        else
            gaps_log "Limiting to path ${LIMIT_PATH}"
            pushd "${LIMIT_PATH}"
            gaps_log "Transferring data"
            if [ "${OLD_GIT_ANNEX_SHA}" == "${NEW_GIT_ANNEX_SHA}" \
                -a \
                "${OLD_MASTER_SHA}" == "${NEW_MASTER_SHA}" \
                -a \
                "${OLD_REMOTE_MASTER_SHA}" == "${NEW_REMOTE_MASTER_SHA}" \
                -a \
                "${OLD_REMOTE_GIT_ANNEX_SHA}" == "${NEW_REMOTE_GIT_ANNEX_SHA}" \
                -a \
                -z "${KONIX_GIT_ANNEX_PERSO_SYNC_FORCE}" ]
            then
                gaps_log "Git branches have not been impacted by sync with ${remote} and force not set"
                gaps_log "-> do not attempt to copy anything"
                #        elif [ -n "$KONIX_GIT_ANNEX_PERSO_SYNC_FAST" ]
                #        then
                #            gaps_log "In fast mode, do not attempt to copy files"
            else
			    if [ -n "${KONIX_GIT_ANNEX_PERSO_SYNC_FORCE}" ]
			    then
				    gaps_log "-> Force set, then force copy/drop"
			    fi
                gaps_log "Getting data from $remote_name (only what is not here)"
                git annex get "${KONIX_GIT_ANNEX_PERSO_SYNC_FAST_ARG}" --auto --from "$remote_name" --not --in here
                gaps_log "Sending data to $remote_name (only what is not there)"
                git annex copy "${KONIX_GIT_ANNEX_PERSO_SYNC_FAST_ARG}" --to "$remote_name" --auto --not --in "$remote_name"
                gaps_log "Dropping data from $remote_name (only what is already there)"
		        git annex drop "${KONIX_GIT_ANNEX_PERSO_SYNC_FAST_ARG}" --auto --from "$remote_name" --in "$remote_name"
                gaps_log "Re syncing $remote_name with me to take into account the changes"
                git annex sync "$remote_name"
                gaps_launch_posthook "${sync_posthook}" "${url}" "sync post hook"
            fi
            popd
        fi
        # launch the post hook and reinit the trap
        gaps_posthook_launch_maybe
        trap "" 0
        RES=0
    done
else
	gaps_log "No remote to sync with"
fi
popd
exit "${RES}"
