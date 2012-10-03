#!/bin/sh

#
# dm-utils.sh - a set of dmenu oriented utilities
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
_WHICH="`which which 2>/dev/null`" || { exit 1; } # Required! ;-)


#
# Configuration variables
#
DMU_DEFAULT_DMENU_OPTS="-nb #303030 -nf #ffffff -sb #8b0000 -sf #ffffff"
DMU_DEFALT_DIRECTORY="${HOME}"


#
# System tools
#
_DMENU="`${_WHICH} dmenu 2>/dev/null`" || { exit 1; }
_FIND="`${_WHICH} find 2>/dev/null`"   || { exit 1; }
_SED="`${_WHICH} sed 2>/dev/null`"     || { exit 1; }
_PWD="`${_WHICH} pwd 2>/dev/null`"     || { exit 1; }
_SORT="`${_WHICH} sort 2>/dev/null`"   || { exit 1; }
_MKDIR="`${_WHICH} mkdir 2>/dev/null`" || { exit 1; }
_CUT="`${_WHICH} cut 2>/dev/null`"     || { exit 1; }


#
# Functions
#

# description: a simple Q&A utility
# invocation: dmu_choice <$question> <$answer1, ..., $answerN>
# return: the choosen answer or empty string otherwise
dmu_choice()
{
    Q="${1}" # question
    A=""     # answers

   test "x${Q}" = "x" && { return 0; }

   while test ! "x${1}" = "x"
    do
        shift
        test "x${A}" = "x" && { A="${1}"; continue; }

        A="${A}\n${1}"
    done

    test "x${A}" = "x" && { return 0; }

    printf "`printf ${A}                                 | \
             ${_DMENU} -p "${Q}" ${DMU_DEFAULT_DMENU_OPTS}`"

    return 1
}

# description: a directory browser and selector
# invocation: dmu_directory_browse [$directory_tip]
# return: the chosen directory name or empty string otherwise
dmu_directory_select()
{
    TD=""          # target directory
    SD=""          # selected directory
    DT="${1}"      # directory tip
    CD="`${_PWD}`" # current directory

    (test ! "x`echo ${DT} | ${_CUT} -c1`" = "x/" && \
     test ! "x${DT}" = "x") && { DT="${CD}/${DT}"; }

    # Keys:
    # * <Enter>           - change to highlighted or prompt-inserted directory
    # * <Shift> + <Enter> - select current directory
    # * <Escape>          - exit
    while :
    do
        # dmenu's prompt
        if test "x${TD}" = "x"; then
            P="Select directory" # dmenu's prompt
        else
            P="Select directory or confirm '${TD}'"
        fi

        PD="" # prepend directories (tip, '.' and '..')

        # If we are on '/', we don't need '..' anymore
        if test ! "x${TD}" = "x/"; then
            test ! "x${DT}" = "x" && { PD="${DT}\n"; }
            PD="${PD}.\n..\n"
        fi

        # Small note: you couldn't choose '~' because it gets
        # quoted so it hasn't recognized as $HOME directory
        SD="`(printf "${PD}"                                && \
              (${_FIND} . -maxdepth 1 -type d                | \
               ${_SORT}                                      | \
               ${_SED} -e 's/^\.\///g; /^\.$/d;'))           | \
             ${_DMENU} -p \"${P}: \" ${DMU_DEFAULT_DMENU_OPTS}`"

        # Escape key, plain exit
        test ! $? -eq 0 && { cd $CD; return 0; }

        # <Shift> + <Enter> - current directory selected, exit from loop
        if test ! "x${TD}" = "x"; then
            test "x${SD}" = "x" && { break; }
        else
            test "x${SD}" = "x" && { continue; }
        fi


        # A bit off-topic, but it's welcome sometimes :-)
        if test ! "x${SD}" = "x" && test ! -d "${SD}"; then
            Q="Directory \`${SD}' does not exist, create it?"
            A="`dmu_choice "${Q}" Yes No`"

            case $A in
                Yes)
                    $_MKDIR -p $SD 2>/dev/null || \
                        { cd $CD; return 1; }
                    ;;
                No)
                    SD="${TD}"
                    ;;
                *)
                    cd $CD
                    return 1
                    ;;
            esac
        fi

        cd $SD 2>/dev/null || { cd $CD; return 1; }
        TD="`${_PWD}`"
    done

    printf "${TD}"
    cd "${CD}"

    return 0
}

# description: a file browser and selector
# invocation: dmu_file_browse <$directory> [$filename_tip]
# return: the chosen filename or empty string if no choice
dmu_file_select()
{
    D="${1}"       # start directory
    TF=""          # target filename
    SF=""          # selected file
    FT="${2}"      # filename tip
    CD="`${_PWD}`" # current directory

    test "x${D}" = "x" && { D="${DMU_DEFAULT_DIRECTORY}"; }
    cd ${D} 2>/dev/null || { return 1; }

    # Keys:
    # * <Enter>           - change to highlighted or prompt-inserted file
    # * <Shift> + <Enter> - select current file
    # * <Escape>          - exit
    while :
    do
        # dmenu's prompt
        if test "x${TF}" = "x"; then
            P="Select filename"
        else
            P="Select filename or confirm '${TF}'"
        fi

        PF="" # prepend filename tip

        test ! "x${FT}" = "x" && { PF="${FT}\n"; }
        test "x${TF}" = "x" && { TF="${FT}"; }

        SF="`(printf "${PF}"                                && \
             (${_FIND} . -maxdepth 1 -type f                 | \
             ${_SORT}                                        | \
             ${_SED} -e 's/^\.\///g;'))                      | \
             ${_DMENU} -p \"${P}: \" ${DMU_DEFAULT_DMENU_OPTS}`"

        # Escape key, plain exit
        test ! $? -eq 0 && { return 1; }

        # <Shift> + <Enter> - current file selected, exit from loop
        (test ! "x${TF}" = "x" && test "x${SF}" = "x") && \
            { break; }

        TF="${SF}"
    done

    printf "${TF}"
    cd "${CD}"

    return 0
}
