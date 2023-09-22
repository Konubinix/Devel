td ( ) {
    if [ "${DISPLAY}" == "" ]
    then
        echo "Setting display" >&2
        sd
    else
        echo "Unseting display" >&2
        ud
    fi
}

function uniq_no_sort {
    # https://stackoverflow.com/questions/11532157/remove-duplicate-lines-without-sorting
    awk '!x[$0]++'
}

function is_on_linux {
    uname -a|grep -i "linux">/dev/null
}

SSH_AGENT_SOURCE=/tmp/ssh-agent-source

function run_ssh_agent_KONIX {
	ssh-agent > $SSH_AGENT_SOURCE
	. $SSH_AGENT_SOURCE
	ssh-add
	res=$?
	if [[ $res != 0 ]]
	then
		rm $SSH_AGENT_SOURCE
	fi
}

function ssh_agent_start_maybe_KONIX {
	if [ -e $SSH_AGENT_SOURCE ]
	then
		. $SSH_AGENT_SOURCE
		if [[ ! $(ps ho command $SSH_AGENT_PID) == "ssh-agent" ]]
		then
			echo "PID de l'agent SSH obsolete, démarrage d'un nouvel agent"
			run_ssh_agent_KONIX
		fi
	else
		echo "Pas de ssh-agent enregistré, on en démarre un"
		run_ssh_agent_KONIX
	fi
}

function trouve_KONIX {
	file="$*"
	find ./ -iname "*$file*"
}

c () {
    cd "$@" && ls
}

u () {
    cd .. && ls
}

p () {
	pushd "$@" && ls
}

b () {
	popd && ls
}

t () {
	cd - && ls
}

m () {
	mkdir -p "$1" && c "$1"
}

private_nav_clean_n_stop () {
    history -c
    history -r
    unset KONIX_PRIVATE_NAV
    trap "" EXIT
}

private_nav () {
    export "KONIX_PRIVATE_NAV=1"
    trap "history -c" EXIT
}

private_nav_from_now () {
    export "KONIX_PRIVATE_NAV=1"
    history -a
    trap "history -c" EXIT
}

is_sourced () {
    echo "Don't use this function, copy it in your script and remove me"
    return 1
    ! [ "$BASH_SOURCE" == "$0" ]
}

my_location () {
    echo "$(dirname "${0}")"
}

my_name () {
    echo "$(basename "${0}")"
}

call_second_like_me () {
    local SECOND="$(konix_find_next_in_path.sh -n "$(my_name)" -f "${0}")"
    if [ "${CALL_SECOND_LIKE_ME_DEBUG}" == "1" ]
    then
        echo "Calling ${SECOND} from $(my_name)" >&2
    fi
    "${SECOND}" "$@"
}

exec_second_like_me () {
    local SECOND="$(konix_find_next_in_path.sh -n "$(my_name)" -f "${0}")"
    exec "${SECOND}" "$@"
}

redis_cache_key ( ) {
    local hash="$(echo "$*"|openssl md5 |cut -f2 -d' ')"
    echo "bash_redis_cache_${hash}"
}

get_redis_cache ( ) {
    local cache_key="$(redis_cache_key "$@")"
    if [ -z "$(konix_redis-cli.sh --raw get "${cache_key}")" ]
    then
        return 1
    fi
    konix_redis-cli.sh --raw get "${cache_key}" 2> /dev/null
}

set_redis_cache ( ) {
    local cache_key="$(redis_cache_key "$@")"
    konix_redis-cli.sh --raw set "${cache_key}" "$(cat)" > /dev/null
}

urlencode() {
    # urlencode <string>
    old_lc_collate=$LC_COLLATE
    LC_COLLATE=C

    local length="${#1}"
    for (( i = 0; i < length; i++ )); do
        local c="${1:i:1}"
        case $c in
            [a-zA-Z0-9.~_-]) printf "$c" ;;
            *) printf '%%%02X' "'$c" ;;
        esac
    done

    LC_COLLATE=$old_lc_collate
}

urldecode() {
    # urldecode <string>

    local url_encoded="${1//+/ }"
    printf '%b' "${url_encoded//%/\\x}"
}

call_history ( ) {
    HISTTIMEFORMAT="" history "$@"
}

history_extract_commandline ( ) {
    local histprefixregexp="......."
    sed "s/${histprefixregexp}//"
}

story () {
    local input="$*"
    # remove the call to story from the history
    if call_history|history_extract_commandline|tail -1|grep -q '^story'
    then
        history -d -1
    fi
    local command="$(call_history|history_extract_commandline|tac|konix_remove_duplicate.py|fzf \
 --query="${input}" \
 --history="${TMPDIR}/story" \
 --history-size=1000000 \
 |trim)"
    if [ -n "${command}" ]
    then
        read -i "${command}" -e -p "$ " command
    fi
    if [ -n "${command}" ]
    then
        # I don't know why the last command get replaced by the call to history
        # -s, but since it happens, let's artificially put again the last
        # command before inserting the new command into the history
        local last_command="$(call_history|history_extract_commandline|tail -1)"
        history -s "${last_command}"
        history -s "${command}"
        eval "${command}"
    else
        echo "Abort"
    fi
}



popline ( ) {
    local file="$1"
    local variable="$2"
    if ! [ -s "${file}" ]
    then
        return 1
    fi
    local thehead="$(head -1 "${file}")"
    (
        TMPDIRPOP="$(mktemp -d)"
        trap "rm -rf '${TMPDIRPOP}'" 0
        tail +2 "${file}" > "${TMPDIRPOP}/temp"
        mv "${TMPDIRPOP}/temp" "${file}"
    )
    read "$2" < <(echo "${thehead}")
}

map ( ) {
    while read line
    do
        "$@" "${line}"
    done
}

konix_die ( ) {
    caller
    if [ -n "$*" ]
    then
        echo "$*"
    fi
    exit 1
}

konix_assert_last_command () {
	local LAST_RES=$?
	local MSG="$*"
	if [ ! $LAST_RES -eq 0 ]
	then
		echo "$MSG">&2
		caller
		exit 1
	fi
}

konix_assert_var () {
    local VAR_VALUE="$(eval "echo \${$(echo $1)}")"
    source konix_assert_var.sh "${VAR_VALUE}"
}

ktns () {
    if test -n "${1}"
    then
        export KTL_NS="$1"
    else
        unset KTL_NS
    fi
}
