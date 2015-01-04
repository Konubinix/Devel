#!/bin/bash

exec am start -a android.intent.action.VIEW -norg.videolan.vlc/.gui.video.VideoPlayerActivity -dhttp://${MPD_HOST}:9635
