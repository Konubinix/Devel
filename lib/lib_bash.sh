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

gpg_agent_start_KONIX () {
	local GPG_INFO_FILE_NAME="${HOME}/.gnupg/gpg-agent-info-${HOSTNAME}"
    if [ -f "$GPG_INFO_FILE_NAME" ] \
		&& pgrep -u "$LOGNAME" gpg-agent > /dev/null 2>&1
	then
		echo "Using current gpg-agent conf from $GPG_INFO_FILE_NAME"
    else
		echo "Starting a new gpg-agent"
		gpg-agent --daemon --enable-ssh-support \
			--write-env-file "$GPG_INFO_FILE_NAME" >/dev/null
	fi
	. "$GPG_INFO_FILE_NAME"
    export GPG_AGENT_INFO
    export SSH_AUTH_SOCK
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

function parse_git_branch {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || return
    echo "("${ref#refs/heads/}")"
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
#
# Reads the content of the file, split its lines and put in the RES global
# variable something of the form "line1"$SEP"line2"$SEP...
# ####################################################################################################
	local FILE="$1"
	local SEP="$2"
	source konix_assert_var.sh "$FILE"
	source konix_assert_var.sh "$SEP"
	source konix_assert.sh "-f '$FILE'"
	RES=""
	IFS=$'\n'
	for LINE in $(<$FILE)
	do
		if [ -z "$RES" ]
		then
			RES="\"$LINE\""
		else
			RES="$RES$SEP\"$LINE\""
		fi
	done
}

export COLOR_BLACK=0
export COLOR_BLUE=4
export COLOR_GREEN=2
export COLOR_CYAN=6
export COLOR_RED=1
export COLOR_PURPLE=5
export COLOR_BROWN=3
export COLOR_LIGHT_GRAY=7
export COLOR_DARK_GRAY=8
export COLOR_LIGHT_BLUE=12
export COLOR_LIGHT_GREEN=10
export COLOR_LIGHT_CYAN=14
export COLOR_LIGHT_RED=9
export COLOR_LIGHT_PURPLE=13
export COLOR_YELLOW=11
export COLOR_WHITE=15

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
		echo "Bad foreground value">&2
		caller
		return 1
	fi
	if [ ! $BG_VALUE -lt 8 ]
	then
		echo "Bad background value">&2
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
    RES="$(printf '%d' "'$1")"
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
