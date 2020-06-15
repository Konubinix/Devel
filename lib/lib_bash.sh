# declare colors to allow scripts with -u to work correctly
export COLOR_RESET=""
export COLOR_BOLD=""
export COLOR_UL=""
export COLOR_FG_BLACK=""
export COLOR_FG_RED=""
export COLOR_FG_GREEN=""
export COLOR_FG_YELLOW=""
export COLOR_FG_BLUE=""
export COLOR_FG_MAGENTA=""
export COLOR_FG_CYAN=""
export COLOR_FG_WHITE=""
export COLOR_BG_BLACK=""
export COLOR_BG_RED=""
export COLOR_BG_GREEN=""
export COLOR_BG_YELLOW=""
export COLOR_BG_BLUE=""
export COLOR_BG_MAGENTA=""
export COLOR_BG_CYAN=""
export COLOR_BG_WHITE=""
export COLOR_BLACK=""
export COLOR_BLUE=""
export COLOR_GREEN=""
export COLOR_CYAN=""
export COLOR_RED=""
export COLOR_PURPLE=""
export COLOR_BROWN=""
export COLOR_LIGHT_GRAY=""
export COLOR_DARK_GRAY=""
export COLOR_LIGHT_BLUE=""
export COLOR_LIGHT_GREEN=""
export COLOR_LIGHT_CYAN=""
export COLOR_LIGHT_RED=""
export COLOR_LIGHT_PURPLE=""
export COLOR_YELLOW=""
export COLOR_WHITE=""

SSH_AGENT_SOURCE=/tmp/ssh-agent-source

function is_on_linux {
    uname -a|grep -i "linux">/dev/null
}

function path2dir_KONIX {
	PWD="$(pwd)"
	test="$(echo "$*" | sed 's-^/--')"
	if [[ "$test" == "$*" ]]
	then
		info="$PWD/$*"
	else
		info="$*"
	fi
	path=$(echo "$info" | sed -r 's-/?[^/]*$--')
	if [[ $path == "" ]]
	then
		echo "./"
	else
		echo $path
	fi
}

function path2file_KONIX {
	echo "$*" | sed -r 's-.*/([^/]*)$-\1-'
}

function remove_trailing_slash_KONIX {
	echo "$1" | sed 's-/$--'
}

function est_nombre_KONIX {
	if [ -z $1 ]
	then
		return 1

	else
		res=$(echo $1 | sed -r 's/[0-9]+//g')
		if [ -z $res ]
		then
			return 0
		else
			return 1
		fi
	fi
}

function timer_on_KONIX {
	timeout=$1
	shift
	command=$*
	# run $command in background, sleep for our timeout then kill the process if it is running

	bash -c "$command" &
	pid=$!

	bash -c "trap 'exit 14' 14; sleep ${timeout}; kill -9 $pid 2> /dev/null" &
	pid_timer=$!
	wait $pid 2> /dev/null
	result_wait=$?
	if [ $result_wait -eq 143 ]; then
		echo "ATTENTION - commande arrétée - timeout de ${timeout} secs atteint."
		return 1
	fi

	kill -s 14 $pid_timer 2> /dev/null > /dev/null
	kill $pid 2> /dev/null
	return $result_wait
}

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

konix_gpg_use () {
    konix_gpg_agent.sh
    unset SSH_AGENT_PID
    if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
        export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh"
    fi
}

function est_connecte_KONIX {
	WGET_WAIT=$1
	SITE="http://www.google.fr/index.html"
	echo "Test de la connexion en tentant de se connecter à $SITE"

	est_nombre_KONIX "$WGET_WAIT"
	if [ ! $? ]
	then
		echo "Mauvaise usage de est_connecte_KONIX"
		return 2
	fi

	timer_on_KONIX $WGET_WAIT "wget $SITE --spider 2> /dev/null > /dev/null"
	return $?
}

function tsocks_KONIX {
	. /usr/bin/tsocks off
	if [ -e ~/.pid_ssh_tsock ]
	then
		pid=$(cat ~/.pid_ssh_tsock)
		if [[ "$(ps ho command $pid)" == "ssh -ND 1080 lourys@ensisun.imag.fr" ]]
		then
			kill -9 $pid
		fi
	fi

	est_connecte_KONIX 2
	if [ $? != 0 ]
	then
		echo "Connecte toi avant de lancer cette commande"
		return 1
	fi
	echo "Connexion par ssh à ensisun"
	ssh -ND 1080 lourys@ensisun.imag.fr &
	pid_ssh=$!
	echo $pid_ssh > ~/.pid_ssh_tsock
	. /usr/bin/tsocks on
}

function trouve_KONIX {
	file="$*"
	find ./ -iname "*$file*"
}

# SVN informations
function parse_svn_revision
{
    revTot=`svn info 2>/dev/null | grep Révision | grep -v dernière | sed -e 's/.*: //'`
    rev=`svn info 2>/dev/null | grep Révision | grep dernière | sed -e 's/.*: //'`
    if [[ ! -z $rev ]]; then echo -n "(r$rev/r$revTot)"; fi
}

function parse_svn_status
{
    if [[ `svn status 2>/dev/null | wc -l` -gt 0 ]]; then echo -n "*"; fi
}

on_windows_p () {
    uname -a|egrep -i "win|mingw">/dev/null
}

cygwin_installed_p () {
    which cygpath.exe > /dev/null 2>&1
}

cygpath2dos () {
    if on_windows_p -a cygwin_installed_p
    then
        cygpath -d "$*"
    fi
}

konix_var_points_to_file_or_null_p () {
    # ####################################################################################################
    # This function checks if the file whose name is stored in variable whose name
    # is in $1 exists, if not so, it replace the content of the variable by ""
    # ####################################################################################################
	local TAGDIR_VAR="$1"
	eval local TAGDIR_FILE="\$$TAGDIR_VAR"
	if [ ! -f "$TAGDIR_FILE" ]
	then
		echo "$TAGDIR_VAR=$TAGDIR_FILE does not point to existing file, empty it" >&2
		eval "$TAGDIR_VAR"=""
	fi
}

konix_file_to_lines () {
    # ####################################################################################################
    # Param 1 : file
    # Param 2 : sep
    # Param 3 : comment
    #
    # Reads the content of the file, split its lines and put in the RES global
    # variable something of the form "line1"$SEP"line2"$SEP... lines beginning with
    # comment will be avoided
    # ####################################################################################################
	local FILE="$1"
	local SEP="$2"
    local comment="$3"
	source konix_assert_var.sh "$FILE"
	source konix_assert_var.sh "$SEP"
	source konix_assert.sh "-f '$FILE'"
	RES=""
	IFS=$'\n'
	for LINE in $(<$FILE)
	do
        if [ -n "$comment" ] && [ "${LINE#${comment}}" != "${LINE}" ]
        then
            continue
        fi
		if [ -z "$RES" ]
		then
			RES="\"$LINE\""
		else
			RES="$RES$SEP\"$LINE\""
		fi
	done
}

konix_int_to_color() {
    #####################################################################################################
    # cf http://en.wikipedia.org/wiki/ANSI_escape_code#Colors
    #
    # |-----------+-------+-----+-------+-----------+------+---------+------+-------|
    # | Intensity | 0     | 1   | 2     | 3         | 4    | 5       | 6    | 7     |
    # |-----------+-------+-----+-------+-----------+------+---------+------+-------|
    # | Normal    | Black | Red | Green | Yellow[7] | Blue | Magenta | Cyan | White |
    # | Bright    | Black | Red | Green | Yellow    | Blue | Magenta | Cyan | White |
    # |-----------+-------+-----+-------+-----------+------+---------+------+-------|
    # Bright = +8
    # Foreground accepts 16 values (light and dark)
    # Background accepts 8 values (only dark)
    # Negative values mean default
    ######################################################################################################
	local FG_VALUE="$1"
	local BG_VALUE="$2"
	if [ ! $FG_VALUE -lt 16 ]
	then
		echo "Bad foreground value $FG_VALUE">&2
		caller
		return 1
	fi
	if [ ! $BG_VALUE -lt 8 ]
	then
		echo "Bad background value $BG_VALUE">&2
		caller
		return 1
	fi
	RES="\033["
	if [ $FG_VALUE -lt 0 ]
	then
		RES="${RES}00;00"
	else
		FG_BRIGHT=$((FG_VALUE/8))
		FG_COLOR=$((FG_VALUE%8))
		RES="${RES}0${FG_BRIGHT};3${FG_COLOR}"
	fi
	if [ $BG_VALUE -lt 0 ]
	then
		RES="${RES}m"
	else
		RES="${RES};4${BG_VALUE}m"
	fi
}

ord() {
    RES="$(printf '%d' "'$1'")"
}

konix_string_to_color_index () {
	local STRING="$1"
	local FUNCT_RES=0
	while [ -n "$STRING" ]
	do
		NEXT_LETTER="${STRING:0:1}"
		STRING="${STRING#?}"
		ord $NEXT_LETTER
		FUNCT_RES=$(( (FUNCT_RES+RES)%16 ))
	done
	RES=$FUNCT_RES
}

help pushd > /dev/null 2>&1
if [ "$?" != "0" ]
then
    pushd ( ) {
        local dirname="$*"
        DIR_STACK="$dirname;${DIR_STACK:-$PWD;}"
        cd "${dirname:?\"missing directory name.\"}"
        echo "$DIR_STACK"
    }

    popd ( ) {
        DIR_STACK="${DIR_STACK#*;}"
        cd "${DIR_STACK%%;*}"
        echo "$PWD"
    }
fi

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

ccache_disable_toggle () {
	if [ "$CCACHE_DISABLE" == "" ]
	then
		CCACHE_DISABLE=yes
	else
		unset CCACHE_DISABLE
	fi
}

pinfo () {
	echo "# $*" >&2
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

source_if_possible () {
    local file="$1"
    if [ -r "$file" ]
    then
	    source "$file"
        return 0
    else
        echo "Cannot load $file"
        return 1
    fi
}

logless () {
	ccze -A < $1 | less -R;
}

logtail () {
	tail -f $1 | ccze -A;
}

private_nav () {
	export KONIX_BASH_PRIVATE_NAV=1
	bash -i
}

import_env_autostash () {
    import_env "" autostash
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
    if [ -n "${CALL_SECOND_LIKE_ME_DEBUG}" ]
    then
        echo "Calling ${SECOND} from $(my_name)" >&2
    fi
    "${SECOND}" "$@"
}

exec_second_like_me () {
    local SECOND="$(konix_find_next_in_path.sh -n "$(my_name)" -f "${0}")"
    if [ -n "${CALL_SECOND_LIKE_ME_DEBUG}" ]
    then
        echo "Executing ${SECOND} from $(my_name)" >&2
    fi
    exec "${SECOND}" "$@"
}

mkvirtualenv_system-site-packages () {
    mkvirtualenv --system-site-packages "$@"
}

mkvirtualenv3 () {
    VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3 mkvirtualenv "$@"
}

mkvirtualenv3_system-site-packages () {
    mkvirtualenv3 --system-site-packages "$@"
}

mkvirtualenv2 () {
    VIRTUALENVWRAPPER_PYTHON=/usr/bin/python2 mkvirtualenv "$@"
}

mkvirtualenv2_system-site-packages () {
    mkvirtualenv2 --system-site-packages "$@"
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

trim ( ) {
    awk '{$1=$1};1'
}

lastcharacter ( ) {
    local input="$*"
    echo "${input: -1}"
}

allbutlastcharacter ( ) {
    local input="$*"
    echo "${input:0:-1}"
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

ipgrep () {
    ps aux | percol | awk '{ print $2 }'
}

ipkill () {
    ps aux | percol | awk '{ print $2 }' | xargs kill "$@"
}

lw () {
    ls|wc -l
}
