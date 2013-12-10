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
r: record given date (current date if set to .) for further incremental
    standup (implicit if -i)
l: see commits from last increment (implicit if -i)
R: reset incremental
q: do nothing and quit, usefull to only reset incremental

What does incremental mean?
Gets the current date from the git config, then shows the log from that date
then, if quitted normally, record the current date into the git config.

Default, show from yesterday or friday if today is monday
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
    echo "Commits will be retrieved since $DATE"
}

set_date () {
    CURRENT_DATE="$1"
    [ "${CURRENT_DATE}" == "." ] && CURRENT_DATE="$(date "+%Y/%m/%d %H:%M:%S")"
    [ -n "${CURRENT_DATE}" ] || CURRENT_DATE="$(date "+%Y/%m/%d %H:%M:%S")"
    echo "Recording ${CURRENT_DATE} in the current git config"
    date_config "${CURRENT_DATE}"
}

rm_date () {
    echo "Removing incremental date"
    git config --remove-section konix.standup
}

DATE="$(yesterworkday)"
while getopts "hd:miRqr:l" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        d)
            DATE="$OPTARG"
            ;;
        m)
            DATE="1 month ago"
            ;;
        i)
            get_date
            set_date "."
            ;;
        R)
            rm_date
            ;;
        l)
            get_date
            ;;
        r)
            set_date "$OPTARG"
            ;;
        q)
            echo "Bye"
            exit 0
            ;;
    esac
done
shift $((OPTIND-1))
git log -w --since="${DATE}" "$@"
