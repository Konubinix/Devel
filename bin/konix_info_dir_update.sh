#!/bin/bash
# update-info-dir
# create a dir file from all installed info files
# Copyright 2009 Norbert Preining
# GPLv2

INFODIR=/usr/share/info

set -e

#
# since user's environment is taken over into root account when sudo-ing
# we don't want that one's user LANGUAGE setting changes the messages in
# the dir file. Unset LANGUAGE and reload /etc/environment to get
# the system wide settings. See bug #536476
unset LANGUAGE
unset LANG
if [ -r /etc/environment ] ; then
  . /etc/environment
fi
if [ -r /etc/default/locale ] ; then
  . /etc/default/locale
fi

Help ()
{
    echo "\
SYNOPSIS: update-info-dir [-h,--help] [info-directory]

(re-)creates the index of available documentation in info format
(the file /usr/share/info/dir) which is usually presented by info browsers
on startup."

    exit 0
}


if [ "$1" = "-h" ] || [ "$1" == "--help" ]; then
    Help
fi

if [ -n "$1" ] ; then
  INFODIR="$1"
fi

if [ ! -d "$INFODIR" ] ; then
  echo "Not a directory: $INFODIR." >&2
  exit 1
fi

if [ -r "$INFODIR/dir" ] ; then
  rm -f "$INFODIR/dir.old"
  cp $INFODIR/dir $INFODIR/dir.old
fi

# we have to remove the dir file not make ginstall-info being surprised
rm -f "$INFODIR/dir"

errors=0
find -L "$INFODIR" -type f | while read file ; do
  case $file in
    */dir|*/dir.gz|*/dir.old|*/dir.old.gz|*-[0-9]|*-[0-9].gz|*-[1-9][0-9]|*-[1-9][0-9].gz|*.png)
      # these files are ignored
      continue
      ;;
    *)
      ginstall-info "$file" "$INFODIR/dir" || {
        errors=$[errors+1]
      }
      ;;
  esac
done

if [ $errors -gt 0 ] ; then
  exec >&2
  echo
  echo "Updating the index of info documentation produced $errors errors."
fi

exit 0

# vim:set expandtab tabstop=2: #
