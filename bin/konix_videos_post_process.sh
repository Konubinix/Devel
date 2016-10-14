#!/bin/bash

set -eu

konix_video_remove_15fps_videos.sh
konix_videos_rename.sh
konix_videos_reencode.sh
