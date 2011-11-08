#!/bin/zsh
######################################################################
#  \file lib_zsh.sh
#
#  \author Konubinix  (konubinix@gmail.com)
#  \date mer. 20:50:19 01/09/2010
######################################################################
function _already_done (
	image="$1"
	testimage="$(echo "$image"|sed 's/-[0-9]\+.png$//')"
	test "$testimage" != "$image"
)

function _ok_to_transparent (
	image="$1"
	testimage="$(echo "$image"|sed 's/img[0-9]\+.png$//')"
	test "$testimage" != "$image"
)

function transparent (
	rep="$1"
	if test -n "$rep"
	then
		echo "cd $rep"
		cd "$rep"
	fi
	for image in **/*.png
	do
		if $(_ok_to_transparent "$image")
		then
			if $(_already_done "$image")
			then
				echo "$image OK"
			else
				echo "$image"
				echo convert "$image" -alpha set -transparent "#FFFFFF" "$(echo	"$image" | sed s/img/img-/ )"
				convert "$image" -alpha set -transparent "#FFFFFF" "$(echo	"$image" | sed s/img/img-/ )"
			fi
		else
			echo "$image not to convert"
		fi
	done
)
