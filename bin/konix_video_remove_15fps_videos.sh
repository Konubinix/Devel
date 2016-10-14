#!/bin/bash

find -\( -name '*webm' -o -name "*MOV" -\) -exec "konix_video_remove_15fps_video.sh" "{}" ";"
