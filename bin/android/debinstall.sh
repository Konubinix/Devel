#!/bin/bash

set -eu

target="${1}"

adb shell 'su -c "mkdir -p /data/bin"'
adb push "${KONIX_DEVEL_DIR}/bin/android/${target}debsetup.sh" /sdcard/
#adb push "${KONIX_DEVEL_DIR}/bin/android/${target}sdcard1_umount.sh" /sdcard/

adb push "${KONIX_DEVEL_DIR}/bin/android/konix_start_ssh_maybe.sh" /sdcard/
adb push "${KONIX_DEVEL_DIR}/bin/android/launch_ssh.sh" /sdcard/
adb push "${KONIX_DEVEL_DIR}/bin/android/install.sh" /sdcard/
adb push "${KONIX_DEVEL_DIR}/bin/android/deb.sh" /sdcard/

adb shell "su -c 'bash /sdcard/install.sh ${target}'"
