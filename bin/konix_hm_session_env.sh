#!/usr/bin/env bash
# Output fully resolved session variables as KEY=VALUE lines.
# Merges plain vars (~/.konix_hm-session-vars.sh) and prepend vars
# (~/.konix_hm-session-prepend-vars.sh) with deduplication.
# Consumers: bashrc, emacs, awesome rc.lua.

set -euo pipefail

declare -A vars

# 1. Plain vars (export KEY=value or export KEY='value')
if [ -f ~/.konix_hm-session-vars.sh ]; then
    while IFS= read -r line; do
        [[ "$line" =~ ^export\ ([^=]+)=(.*)$ ]] || continue
        key="${BASH_REMATCH[1]}"
        val="${BASH_REMATCH[2]}"
        # Strip surrounding single quotes if present
        if [[ "$val" =~ ^\'(.*)\'$ ]]; then
            val="${BASH_REMATCH[1]}"
        fi
        vars["$key"]="$val"
    done < ~/.konix_hm-session-vars.sh
else
    echo "WARNING: ~/.konix_hm-session-vars.sh not found" >&2
fi

# 2. Prepend vars (deduplicated)
if [ -f ~/.konix_hm-session-prepend-vars.sh ]; then
    while IFS='=' read -r key val; do
        [[ -z "$key" || "$key" == \#* ]] && continue
        local_cur="${vars[$key]:-${!key:-}}"
        if [ -n "$local_cur" ]; then
            combined="$val:$local_cur"
        else
            combined="$val"
        fi
        vars["$key"]="$(echo "$combined" | tr ':' '\n' | awk '!seen[$0]++' | paste -sd:)"
    done < ~/.konix_hm-session-prepend-vars.sh
else
    echo "WARNING: ~/.konix_hm-session-prepend-vars.sh not found" >&2
fi

# 3. Output
for key in "${!vars[@]}"; do
    printf "%s='%s'\n" "$key" "${vars[$key]//\'/\'\\\'\'}"
done
