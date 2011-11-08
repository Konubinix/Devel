#!/bin/bash

pushd "$1" >/dev/null
PATHA="`pwd`"
popd >/dev/null
pushd "$2" >/dev/null
PATHB="`pwd`"
popd >/dev/null
ARGC=$#

# Change the internal field separator ($IFS) to only separate on newline:
IFS=$'\n'

USAGE=$( cat <<__EOF

Synchronize files under two local paths (with help of git).

Usage:
`basename $0` "path A" "path B"

__EOF
)

if [ $ARGC -lt 2 -o $ARGC -gt 2 -o "$PATHA" = "$PATHB" ]; then
  echo "$USAGE";
  exit 1;
fi

GIT=`which git`
if [ "${GIT}x" = "x" ]; then
  echo Git not found! You need to install git first.
  exit 1;
fi


function confirm {
  while true
  do
    #echo -n "Please confirm (y or n) :"
    read CONFIRM
    case $CONFIRM in
      y|Y|YES|yes|Yes) CONFIRM=1; break ;;
      n|N|no|NO|No) CONFIRM=0; break ;;
      *) echo Please enter only y or n
    esac
  done
  return $CONFIRM;
}
# example:
#confirm
#CONFIRMED=$?
# now $CONFIRMED has either value 0 (typed No) or 1 (typed Yes)

function git_addremove_commit {
  if [ `$GIT ls-files --modified --deleted --other | wc -l` -gt 0 ]; then
    $GIT add -A .
    if [ `$GIT ls-files --deleted | wc -l` -gt 0 ]; then
      $GIT ls-files -z --deleted | xargs -0 $GIT rm;
    fi;
  fi
  if [ `$GIT diff --staged | wc -l` -gt 0 ]; then
    $GIT commit -q -a -m "sync commit"
  fi
}

HASGIT_PATHA=0
HASGIT_PATHB=0
if [ -d "${PATHA}/.git" ]; then HASGIT_PATHA=1; fi
if [ -d "${PATHB}/.git" ]; then HASGIT_PATHB=1; fi

if [ $HASGIT_PATHA -eq 0 -a $HASGIT_PATHB -eq 1 ]; then
  echo Git repository not found in \"$PATHA\" but exists in \"$PATHB\"!
  echo You maybe want to switch path order?
  exit 1;
fi

pushd "${PATHA}" >/dev/null
if [ $HASGIT_PATHA -eq 0 ]; then
  echo
  echo Please enter UNIQUE description for "$PATHA"
  echo -n "It will be used to describe potential future conflict sources: "
  read DESC
  $GIT init
  $GIT config user.name "$DESC"
  $GIT config user.email ""
  $GIT add -A .
  $GIT commit -q -a -m "sync initial commit"
fi
echo -n "Checkpointing the state in $PATHA ..."
git_addremove_commit
echo " done."
popd >/dev/null

pushd "${PATHB}" >/dev/null;
if [ $HASGIT_PATHB -eq 0 ]; then
  echo -n "Performing initial synchronization from $PATHA to $PATHB ..."
  cp -a "${PATHA}"/.git ./
  echo " done."
  echo
  echo Please enter UNIQUE description for "$PATHB"
  echo -n "It will be used to describe potential future conflict sources: "
  read DESC
  $GIT config user.name "$DESC"
  $GIT config user.email ""
  #if [ `$GIT ls-files --deleted --other | wc -l` -gt 0 ]; then
    echo "checking for new files in $PATHB"
    if [ `$GIT ls-files --other | wc -l` -gt 0 ]; then
      echo
      echo These files in "$PATHB" do not exist in "$PATHA".
      echo "----------------------"
      $GIT ls-files --other
      echo "----------------------"
      echo -n "Do you want to add them? Type 'Yes' to add them, or 'No' to delete them: "
      confirm; CONFIRMED=$?
      if [ $CONFIRMED -eq 1 ]; then 
        echo -n "Adding all new files..."
        $GIT add -A .
      else
        echo -n "Removing new files..."
        $GIT ls-files -z -o | xargs -0 rm -f
      fi
      echo " done."
    fi
    echo "checking for deleted files in $PATHB"
    if [ `$GIT ls-files --deleted | wc -l` -gt 0 ]; then
      echo
      echo These files in "$PATHA" do not exist in "$PATHB". 
      echo "----------------------"
      $GIT ls-files --deleted
      echo "----------------------"
      echo -n "Do you want to add them? Type 'Yes' to add them, or 'No' to delete them: "
      confirm; CONFIRMED=$?
      if [ $CONFIRMED -eq 1 ]; then 
        echo -n "Adding all new files..."
        $GIT ls-files -z --deleted | xargs -0 $GIT checkout;
      else
        echo -n "Removing new files..."
        $GIT ls-files -z --deleted | xargs -0 $GIT rm;
      fi
      echo " done."
    fi;
  #fi
fi
echo -n "Checkpointing the state in $PATHB ..."
git_addremove_commit
echo " done."
$GIT pull -q "${PATHA}"
if [ `$GIT ls-files --unmerged | wc -l` -gt 0 ]; then
  for F in "`$GIT ls-files --unmerged | cut -f2 | sort -u`"; do
    echo "Processing conflicting file: $F"
    FEXT="${F##*.}"
    FBASE="${F%.*}"
    DESC="`$GIT config --get user.name`"
    $GIT checkout --ours "$F"
    mv -v "$F" "$FBASE (Conflicting version from $DESC).$FEXT"
    $GIT checkout --theirs "$F"
  done
  git_addremove_commit
fi
popd >/dev/null;

pushd "${PATHA}" >/dev/null;
  $GIT pull -q "${PATHB}"
popd >/dev/null;

