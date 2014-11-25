#!/system/xbin/env bash

CARD_AUTO_MOUNTED="/storage/sdcard1 /mnt/fuse/sdcard1"

mkdir_maybe ( ) {
    [ -d "$1" ] || mkdir -p "$1"
}

is_mounted ( ) {
    mount | grep -q " $1 "
}

say_and_do ( ) {
    echo "$*"
    "$@"
}

echo "umount the automatic mount of the card"
for auto_mounted in ${CARD_AUTO_MOUNTED}
do
    ok=1
    if ! is_mounted "${auto_mounted}"
    then
        echo "${auto_mounted} already unmounted"
        continue
    fi
    for i in {1..5}
    do
        echo "Attempt $i to umount ${auto_mounted}"
        say_and_do umount "${auto_mounted}"
        ok="$?"
        if [ "$ok" == "0" ]
        then
            echo "Umounted ${auto_mounted}"
            break
        else
            echo "Could not umount ${auto_mounted}, waiting a bit"
            sleep 5
        fi
    done
    if [ "$ok" != "0" ]
    then
        echo "Could not umount ${auto_mounted}, exiting"
        exit 1
    fi
done
