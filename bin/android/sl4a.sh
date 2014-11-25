#! /system/xbin/env bash
set -x
#RUNNING="$(ps|grep android_scripting|wc -l)"
#if [ "$RUNNING" != "0" ]
#then
#  echo "sl4a already running"
#  exit 1
#fi
pkill android_scripting
PORT="${1:-45001}"
export HOSTNAME=localhost
echo "Running on port $PORT"

am start --user 0 -a com.googlecode.android_scripting.action.LAUNCH_SERVER -n com.googlecode.android_scripting/.activity.ScriptingLayerServiceLauncher --ei com.googlecode.android_scripting.extra.USE_SERVICE_PORT $PORT
