#!/bin/bash

source "${KONIX_LIB_DIR}/lib_bash.sh"

function yesterworkday()
{
    if [[ "1" == "$(date +%w)" ]]
    then
        echo "last friday"
    else
        echo "yesterday"
    fi
}

usage ( ) {
    cat<<EOF
$0 [-h] [-d DATE] [-m]
h: show this
m: from last month
d: from DATE
i: incremental
s: displays the status of the incremental
v: validate increment, record the last seen date for next incremental
r: record given date (current date if set to .) for further incremental
    standup
R: reset incremental
q: do nothing and quit, usefull to only reset incremental
k: lauch gitk instead of git log
y: from yesterworkday

What does incremental mean?
Gets the current date from the git config, then shows the log from that date
then, if quitted normally, record the current date into the git config.
EOF
}

date_config () {
    git config konix.standup.last "$@"
}

get_date () {
    DATE="$*"
    [ -n "${DATE}" ] || DATE="$(date_config)"
    [ -n "${DATE}" ] || DATE="1970/01/01"
    konix_assert_var DATE
    echo "Last known reviewed date is '${DATE}'"
}

now () {
    date "+%Y/%m/%d %H:%M:%S"
}

record_now () {
    NOW="$(now)"
    git config konix.standup.now "$NOW"
    echo "Recorded now to be $NOW (use validate to commit that date)"
}

get_recorded_now () {
    NOW="$(git config konix.standup.now)"
    echo "Last known NOW is $NOW"
}

rm_recorded_now () {
    git config --unset-all konix.standup.now
    echo "Remove the stored value of NOW"
}

set_date () {
    CURRENT_DATE="$1"
    [ "${CURRENT_DATE}" == "." ] && CURRENT_DATE="$(now)"
    [ -n "${CURRENT_DATE}" ] || CURRENT_DATE="$(now)"
    echo "Recording '${CURRENT_DATE}' in the current git config last date"
    date_config "${CURRENT_DATE}"
}

reset () {
    echo "Removing incremental date"
    git config --remove-section konix.standup
}

DATE=""
COMMAND="git log"
while getopts "kshyvd:miRqr:l" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        d)
            DATE="$OPTARG"
            ;;
        k)
            COMMAND="gitk"
            ;;
        m)
            DATE="1 month ago"
            ;;
        i)
            get_date
            record_now
            ;;
        v)
            get_recorded_now
            konix_assert_var NOW
            set_date "$NOW"
            rm_recorded_now
            ;;
        R)
            reset
            ;;
        s)
            get_recorded_now
            if [ -n "${NOW}" ]
            then
                echo "Think about validating ($0 -v -q)"
            fi
            get_date
            ;;
        r)
            set_date "$OPTARG"
            ;;
        q)
            echo "Bye"
            exit 0
            ;;
        y)
            DATE="$(yesterworkday)"
            ;;
    esac
done
shift $((OPTIND-1))
if [ -z "${DATE}" ]
then
    echo "No date set, use -i, -m or -y"
    usage
    exit 2
fi

$COMMAND --name-status --reverse -w --since="${DATE}" $(git config konix.standup.extra) ${GIT_STANDUP_EXTRAS} "$@"
