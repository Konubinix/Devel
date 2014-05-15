#!/bin/bash

source "${KONIX_LIB_DIR}/lib_bash.sh"

echo "${COLOR_FG_CYAN}##### The mails to re check are:${COLOR_RESET}"
notmuch search tag:deleted and tag:draft and not '--text follows this line--'
echo "${COLOR_FG_CYAN}##### The mails to delete almost without risk are:${COLOR_RESET}"
notmuch search tag:deleted and tag:draft and '--text follows this line--'
echo "${COLOR_FG_CYAN}##### Other mails to delete (pay attention to those) are:${COLOR_RESET}"
notmuch search tag:deleted and not tag:draft
echo "${COLOR_FG_CYAN}Delete them ?${COLOR_RESET}"
read y
if [ "$y" == "y" ]
then
	notmuch search --output=files tag:deleted | while read mail_file
	do
		rm -v "$mail_file"
	done
	echo "Re sync notmuch"
	notmuch new
else
	echo "Doing nothing"
fi
