#!/bin/bash
set -o errexit # -e
set -o errtrace # -E
set -o nounset # -u
set -o pipefail
shopt -s inherit_errexit

# ctrl-c
trap "exit 2" SIGINT
trap "exit 3" SIGQUIT

# taken from https://gist.github.com/guidocella/a272b6e68f9c44532b011f6596e95c61#file-dump-cookies-sh
{
    echo '# Netscape HTTP Cookie File' # needed by youtube-dl

    # There is no attempt to url encode $1, but SQLite already handles
    # characters like spaces, so only ? % and # should cause issues.
    sqlite3 -separator $'\t' "file:${XDG_DATA_HOME}/qutebrowser/webengine/Cookies?nolock=1" "
SELECT
    host_key,
    IIF(host_key LIKE '.%', 'TRUE', 'FALSE'),
    path,
    IIF(is_secure, 'TRUE', 'FALSE'),
    IIF(expires_utc == 0, 0, expires_utc / 1000000 - 11644473600),
    name,
    value
FROM cookies;"
} > $XDG_RUNTIME_DIR/cookies.txt

# expires_utc is converted from the Windows NT Time Format to a UNIX timestamp
