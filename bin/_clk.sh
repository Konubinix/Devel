#!/bin/bash -eu

list_to_choice () {
    echo "[$(sed -r 's-(.+)- "\1"-'| paste -s - -d,)]"
}

trim () {
    awk '{$1=$1};1'
}
