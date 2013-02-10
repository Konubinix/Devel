#!/bin/sh

#
# dm-dm.sh - a dmenu oriented wget helper
#
# Copyright (C) 2011 by Alessandro Massignan <ff0000.it@gmail.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#
# Credits:
#  * inspired by lich-download.sh [http://www.uzbl.org/wiki/lich-download.sh]
#
# Thanks:
#  * keis, bct from #uzbl - testing
#
_WHICH="`which which 2>/dev/null`" || { exit 1; } # Required! ;-)


#
# Configuration variables
#
WGET_OPTS="--user-agent Firefox"
TMP_DIRECTORY="/tmp"
DEFAULT_DOWNLOAD_DIRECTORY="${XDG_DOWNLOAD_DIR:-$HOME}"


#
# Sourcing utilities
#
_DMUTILSSH="`${_WHICH} dm-utils.sh 2>/dev/null`" || { exit 1; }
. $_DMUTILSSH                                    || { exit 1; }


#
# System tools
#
_WGET="`${_WHICH} wget 2>/dev/null`"         || { exit 1; }
_CAT="`${_WHICH} cat 2>/dev/null`"           || { exit 1; }
_BASENAME="`${_WHICH} basename 2>/dev/null`" || { exit 1; }
_DIRNAME="`${_WHICH} dirname 2>/dev/null`"   || { exit 1; }
_MKTEMP="`${_WHICH} mktemp 2>/dev/null`"     || { exit 1; }
_MKFIFO="`${_WHICH} mkfifo 2>/dev/null`"     || { exit 1; }
_RM="`${_WHICH} rm 2>/dev/null`"             || { exit 1; }
_CUT="`${_WHICH} cut 2>/dev/null`"           || { exit 1; }
_SLEEP="`${_WHICH} sleep 2>/dev/null`"       || { exit 1; }
_KILL="`${_WHICH} kill 2>/dev/null`"         || { exit 1; }
_PS="`${_WHICH} ps 2>/dev/null`"             || { exit 1; }


#
# Functions
#
usage()
{
    S=""

    if test ! "x${1}" = "x"; then
        S=": ${1}"
    fi

    PN="`${_BASENAME} ${0}`"

    $_CAT <<EOF
$PN $S
Try \`$PN --help\` for further informations.
EOF
}

help()
{
    PN="`${_BASENAME} ${0}`"

    $_CAT <<EOF
Usage: $PN <OPTIONS>
Handles wget downloads

Options:
  -s,--start <URL>          start downloading the passed URL
  -p,--pause <WGET_PID>     pause a download
  -r,--resume <WGET_PID>    resume a download
  -c,--cancel <WGET_PID>    cancel a download
  -l,--list                 list downloads
  -h,--help                 display these information
  -g,--gui                  launch the dmenu based gui
EOF
}

# Sanitize URL encoded characters and substitute multiple white spaces to '_'
sanitize_filename()
{
    printf "${1}"                                               | \
    ${_SED} -r                                                    \
            -e 's/[_%]20/\_/g; s/[_%]22/\"/g; s/[_%]23/\#/g;'     \
            -e 's/[_%]24/\$/g; s/[_%]25/\%/g; s/[_%]26/\&/g;'     \
            -e 's/[_%]28/\(/g; s/[_%]29/\)/g; s/[_%]2C/\,/g;'     \
            -e 's/[_%]2D/\-/g; s/[_%]2E/\./g; s/[_%]2F/\//g;'     \
            -e 's/[_%]3C/\</g; s/[_%]3D/\=/g; s/[_%]3E/\>/g;'     \
            -e 's/[_%]3F/\?/g; s/[_%]40/\@/g; s/[_%]5B/\[/g;'     \
            -e 's/[_%]5C/\\\\/g; s/[_%]5D/\]/g; s/[_%]5E/\^/g;'   \
            -e 's/[_%]5F/\_/g; s/[_%]60/\`/g; s/[_%]7B/\{/g;'     \
            -e 's/[_%]7C/\|/g; s/[_%]7D/\}/g; s/[_%]7E/\~/g;'     \
            -e 's/[_%]2B/\+/g; s/\ \ */\_/g;'

    return 0
}

# Start a backgrounded wget instance and statistics retrivial while loop
dm_start()
{
    URL="${1}"
    FILENAME="`sanitize_filename "\`printf "${URL}" | \
               ${_SED} -e 's/^.*\///'\`"`"

    # Select output directory and file name with
    # dmu_{file,directory}_select() helpers from dm-utils.sh
    # Cursed shall be who uses white spaces in directory and
    # file names ;-D (i've not tested this condition yet :-P)
    OUTPUT=""
    OUTPUT_DIRECTORY=""
    OUTPUT_FILENAME=""

    while :
    do
        test "x${OUTPUT_DIRECTORY}" = "x" && \
            { OUTPUT_DIRECTORY="${DEFAULT_DOWNLOAD_DIRECTORY}"; }

        OUTPUT_DIRECTORY="`dmu_directory_select "${OUTPUT_DIRECTORY}"`"

        test "x${OUTPUT_DIRECTORY}" = "x" && { return 0; }

        test "x${OUTPUT_FILENAME}" = "x" && \
            { OUTPUT_FILENAME="${FILENAME}"; }

        OUTPUT_FILENAME="`dmu_file_select "${OUTPUT_DIRECTORY}" \
                                          "${OUTPUT_FILENAME}"`"

        test "x${OUTPUT_FILENAME}" = "x" && { continue; }
        break
    done

    OUTPUT="${OUTPUT_DIRECTORY}/${OUTPUT_FILENAME}"

    # Ouch! Output file already exists, handling it
    if test -r $OUTPUT; then

        # Checking if other download process is writing to the same file
        if test ! "x`echo ${TMP_DIRECTORY}/dm-info-*`" = \
                  "x${TMP_DIRECTORY}/dm-info-*"; then

            for i in `echo $TMP_DIRECTORY/dm-info-*`
            do
                eval "`${_CAT} ${i}`"
                test "x${OUTPUT}" =                                     \
                     "x${DM_OUTPUT_DIRECTORY}/${DM_OUTPUT_FILENAME}" && \
                    { return 1; }
            done
        fi

        Q="File \`${OUTPUT}' already exists. Would you like to "
        A="`dmu_choice "${Q}" Overwrite Continue Cancel`"

        case $A in
            Overwrite)
                $_RM -f $OUTPUT 2>/dev/null || { return 1; }
                ;;
            Continue)
                WGET_OPTS="${WGET_OPTS} --continue"
                ;;
            *)
                return 1
                ;;
        esac
    fi

    # Named pipe for wget statistics
    STATS_PIPE="`${_MKTEMP} -u --tmpdir="${TMP_DIRECTORY}" dm-stats-XXXXXX`"

    # Hoping Murphy doesn't create the same file in the between :-P
    $_MKFIFO -m 0600 $STATS_PIPE

    # Information file keeps all data we need
    INFO_FILE="`${_MKTEMP} --tmpdir="${TMP_DIRECTORY}" dm-info-XXXXXX`"
    WGET_PID=""

    # Finally we could start downloading
    $_WGET $WGET_OPTS -O "${OUTPUT}" "${URL}" >$STATS_PIPE 2>&1 &
    WGET_PID=$!

    # Basis information file
    $_CAT >$INFO_FILE <<EOF
DM_WGET_PID="$WGET_PID"
DM_URL="$URL"
DM_OUTPUT_DIRECTORY=`$_DIRNAME $OUTPUT`
DM_OUTPUT_FILENAME=`$_BASENAME $OUTPUT`
DM_DOWNLOADED_SIZE=""
DM_PERCENTAGE=""
DM_SPEED=""
DM_ETA=""
EOF

    # This is the loop in which we gather wget statistics from
    # the named pipe and update the information file
    {
        ${_CAT} $STATS_PIPE | \
        while read L
        do
            # Ok, regexp from hell time! :-)
            L="`echo "${L}" | \
                ${_SED} -nu 's/^[\ \t]*\([0-9][^\ \t]*\)[^0-9]*\([0-9][0-9]*%\)[\ \t][\ \t]*\([0-9][^\ \t]*\)[\ \t][\ \t]*\([0-9][^\ \t]*\).*$/DS\=\"\1\";\ P\=\"\2\";\ S\=\"\3\";\ E\=\"\4\";/p;'`"

            eval "${L}"
            eval "`${_CAT} ${INFO_FILE}`"

            # Uploading (basically a rewritten, no time for file parsing :-P)
            # information file everytime the percentage change (so 100
            # writes). I choose this way because i don't need to be updated
            # on every wget's output line... beeing on percentage is fine
            # for me.
            # Feel free to change this if you want :-)
            if test ! "x${DM_PERCENTAGE}" = "x${P}"; then
                $_CAT >$INFO_FILE <<EOF
DM_WGET_PID="$DM_WGET_PID"
DM_URL="$DM_URL"
DM_OUTPUT_DIRECTORY="$DM_OUTPUT_DIRECTORY"
DM_OUTPUT_FILENAME="$DM_OUTPUT_FILENAME"
DM_DOWNLOADED_SIZE="$DS"
DM_PERCENTAGE="$P"
DM_SPEED="$S"
DM_ETA="$E"
EOF
            fi
        done
        # When wget ends the download or it's killed by someone else we've
        # to remove the resources we used... I can't live with forgotten
        # garbage.
        $_RM -f $INFO_FILE $STATS_PIPE 2>/dev/null || { return 0; }
    } &

    return 0
}

# Produce a list of wget instances spawned by $0
dm_list()
{
    # Sometimes 'echo' is a fine replacement to 'ls'
    test "x`echo ${TMP_DIRECTORY}/dm-info-*`" = \
         "x${TMP_DIRECTORY}/dm-info-*" &&       \
        { return 0; }

    for i in `echo $TMP_DIRECTORY/dm-info-*`
    do
        eval "`${_CAT} ${i}`"

        STATUS="`test x"\`${_PS} h -o state -p ${DM_WGET_PID}\`" = "xT" && \
                 { printf "paused"; }`"

        test "x${STATUS}" = "xpaused" && { DM_SPEED="-"; DM_ETA="-"; }

        printf "%s %s/%s [%s, %s, %s, %s] %s\n" \
            $DM_WGET_PID                        \
            $DM_OUTPUT_DIRECTORY                \
            $DM_OUTPUT_FILENAME                 \
            $DM_DOWNLOADED_SIZE                 \
            $DM_PERCENTAGE                      \
            $DM_SPEED                           \
            $DM_ETA                             \
            $STATUS
    done

    return 0
}

# Pause a download
dm_pause()
{
    WGET_PID="${1}"

    $_KILL -s STOP $WGET_PID || { return 1; }

    return 0
}

# Resume a download, if we resume an active download (so a process)
# the CONT signal is harmless and it'll be ignored [resource kill(1)]
dm_resume()
{
    WGET_PID="${1}"

    $_KILL -s CONT $WGET_PID || { return 1; }

    return 0
}

# Cancel a download, we don't perform file's removing because
# we're shell guys and we know 'rm' exists :-D
dm_cancel()
{
    WGET_PID="${1}"

    $_KILL -9 $WGET_PID || { return 1; }

    return 0
}

# A simple dmenu driven GUI to handle dm_list, dm_pause, dm_resume and
# dm_cancel
dm_gui()
{
    while :
    do
        # Calculating number of lines for dmenu's vertical displaying
        LINES="`dm_list | ${_SED} -n '$='`"

        # No lines, no downloads so a GUI is useless
        test "x${LINES}" = "x" && { konix_display.py "Nothing to download"; return 0; }

        # Pick a download
        PROMPT="Select a download:"
        SELECTED_DOWNLOAD="`dm_list                            | \
                            ${_DMENU} ${DMU_DEFAULT_DMENU_OPTS}  \
                            -l ${LINES} -p "${PROMPT}"`"

        # Keys:
        # * <Enter>           - select a download
        # * <Shift> + <Enter> - refresh the list
        # * <Escape>          - exit
        if test $? -eq 0; then
            test "x${SELECTED_DOWNLOAD}" = "x" && { continue; }
        else
            return 0
        fi

        WGET_PID="`echo ${SELECTED_DOWNLOAD} | ${_CUT} -d' ' -f1`"
        Q="Would you like to: "
        A="`dmu_choice "${Q}" Pause Resume Cancel`"

        if test ! $? -eq 0; then
            test "x${A}" = "x" && { continue; }
        else
            return 0
        fi

        case $A in
            Pause)
                dm_pause $WGET_PID || { return 1; }
                ;;
            Resume)
                dm_resume $WGET_PID || { return 1; }
                ;;
            Cancel)
                dm_cancel $WGET_PID || { return 1; }
                $_SLEEP 1s
                ;;
            *)
                # NOP
                ;;
        esac
    done

    return 0
}


#
# Main
#
test "x${1}" = "x" && { usage "no option specified"; exit 1; }

# Parsing command line arguments
while test ! "x${1}" = "x"
do
    case "${1}" in
        -s|--start)
            shift
            dm_start "${1}" || { exit 1; }
            break
            ;;
        -p|--pause)
            shift
            dm_pause "${1}" || { exit 1; }
            break
            ;;
        -r|--resume)
            shift
            dm_resume "${1}" || { exit 1; }
            break
            ;;
        -c|--cancel)
            shift
            dm_cancel "${1}" || { exit 1; }
            break
            ;;
        -l|--list)
            dm_list || { exit 1; }
            break
            ;;
        -g|--gui)
            dm_gui || { exit 1; }
            break
            ;;
        -h|--help)
            help || { exit 1; }
            break
            ;;
        *)
            usage "unrecognized option \`${1}'"
            exit 1
            ;;
    esac
    shift
done

# All done, see you on the next download! 8^)
exit 0
