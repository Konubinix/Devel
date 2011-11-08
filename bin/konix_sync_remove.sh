#!/bin/bash

source _konix_sync_functions.sh

konix_sync_list.sh
echo "Remove which ?"
konix_sync_select_secondary_dir
sed -i "${RES}d" "$KONIX_SYNC_DIR_FILE"
echo "New list"
konix_sync_list.sh
