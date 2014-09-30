#!/bin/bash

source "git-annex-perso_lib.sh"

FILTER="$1"

failure=0

inc_failure_and_continue () {
    failure=$((failure + 1))
    continue
}

if [ -d ".gitannexremotes" ]
then
    gaps_log "Initing the remotes"
    # the following commands set useful variables
    gaps_compute_contexts
	gaps_compute_remotes "${FILTER}"
	for remote in ${remotes}
    do
        gaps_show_date_maybe
        gaps_extract_remote_info_or_continue "${contexts}" "${remote}"
        if gaps_remote_initialized_p "${remote}"
        then
            gaps_warn_and_continue "Aborting initialization because $remote_name is already a remote"
        fi
		if [ "$type" == "rsync" ]
		then
			if ! gaps_ask_for_confirmation "Initing remote $remote_name of type $type ?"
			then
				inc_failure_and_continue
			fi
			git annex enableremote "$remote"
			continue
		fi
		if [ "$type" != "ssh" ] && [ "$type" != "git" ] && [ "$type" != "local" ]
		then
			gaps_error "I don't know how to initialize the remote $remote of type $type"
			inc_failure_and_continue
		fi
		# remote in ssh mode
		if ! gaps_ask_for_confirmation "Initing remote $remote_name
to url $url ?"
		then
			inc_failure_and_continue
		fi
        gaps_launch_availhook_or_continue "${availhook}" "${url}" "${remote_name}"

        gaps_log "Initializing remote '$remote_name'"
		# perform the substitution in url
        git remote add "$remote_name" "$url"
        # set a trap to remove it in case the user aborts now
        trap "gaps_log \"Removing remote $remote_name\" ; git remote remove \"$remote_name\"" 0
        gaps_log "attempt to pull from $remote_name"
        git pull "$remote_name" master
        if [ $? -ne 0 ]
        then
            gaps_log "Could not pull from $remote_name, may be it does not exist yet"
            if gaps_ask_for_confirmation "Clone self to $url ?"
            then
                gaps_log "Cloning . to $url"
                git clone --bare . "$url" || {
                    gaps_log "Aboooort"
				    git remote remove "${remote_name}"
                    # reinit the trap.
                    trap "" 0
                    inc_failure_and_continue
                }
            elif gaps_ask_for_confirmation "Keep $remote_name anyway ?"
            then
                gaps_log "Keeping remote ${remote_name}"
            else
                gaps_log "Aboooort"
				git remote remove "$remote_name"
                # reinit the trap.
                trap "" 0
                inc_failure_and_continue
            fi
        fi
        if [ $? -ne 0 ]
        then
            gaps_log "Aboooort"
			git remote remove "$remote_name"
            # reinit the trap.
            trap "" 0
            inc_failure_and_continue
        fi

        gaps_setup_context_independent_remote_info

        # reinit the trap.
        trap "" 0
        git annex describe "${remote}" "${remote}"
        # set the readonly information
        if [ -f "${readonly}" ]
        then
            gaps_log "Setting the readonly flag"
            git config remote."${remote}".annex-readonly true
        fi
        git annex sync "$remote_name"
        gaps_show_date_maybe
        if [ -f "${posthook}" ]
        then
            gaps_log "Launching the posthook"
            bash -x "${posthook}" "$url"
            if [ $? -ne 0 ]
            then
                gaps_log "Failed to launch the posthook"
            fi
        fi
    done
else
    gaps_log "No remote to init"
fi
exit $failure
