#!/usr/bin/env bash
# Helper to find bash-completion files portably (FHS and NixOS)

_find_completion() {
    local cmd="$1"
    local f bin comp_dir
    # Try FHS path first (Debian, Arch, etc.)
    for f in "/usr/share/bash-completion/completions/$cmd" "/usr/share/bash-completion/completions/$cmd.bash"; do
        [[ -f "$f" ]] && { echo "$f"; return; }
    done
    # Try resolving from the command's Nix store path
    bin="$(readlink -f "$(type -P "$cmd" 2>/dev/null)")" 2>/dev/null
    if [[ -n "$bin" ]]; then
        comp_dir="$(dirname "$bin")/../share/bash-completion/completions"
        for f in "$comp_dir/$cmd" "$comp_dir/$cmd.bash"; do
            [[ -f "$f" ]] && { echo "$f"; return; }
        done
    fi
    # Try bash-completion's own base directory (NixOS: completions live in the
    # bash-completion package, not alongside the command's binary)
    if [[ -n "${_comp__base_directory-}" ]]; then
        for f in "$_comp__base_directory/completions/$cmd" "$_comp__base_directory/completions/$cmd.bash"; do
            [[ -f "$f" ]] && { echo "$f"; return; }
        done
    fi
    return 1
}
