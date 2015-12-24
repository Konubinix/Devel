#! /system/bin/env bash

am broadcast --user 0 -a android.intent.action.MEDIA_MOUNTED -d file:///mnt/sdcard
