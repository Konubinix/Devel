#!/bin/bash
DISPLAY=:0.0
BACKGROUND_PATH='~/.fluxbox/styles/perso/backgrounds/'
SWITCHER=`which fbsetbg`

NB=$(ls -1 $BACKGROUND_PATH | wc -l)
NUM=$(( ($RANDOM % $NB) + 1))
CHEMIN=$(ls -1 $BACKGROUND_PATH | grep -n . | grep ^$NUM | sed "s/^$NUM://")
echo $DISPLAY
exec $SWITCHER "$BACKGROUND_PATH$CHEMIN"