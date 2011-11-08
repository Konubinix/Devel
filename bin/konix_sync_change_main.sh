#!/bin/bash
source _konix_sync_variables.sh
source _konix_sync_functions.sh

konix_sync_list
echo "What number of the secondary list do you want ?"
konix_sync_select_secondary_dir
OLD_MAIN="$(cat "$KONIX_SYNC_MAIN_DIR_FILE")"
NEW_MAIN="$(awk "NR==$RES" "$KONIX_SYNC_DIR_FILE")"
echo "$OLD_MAIN" >> "$KONIX_SYNC_DIR_FILE"
echo "$NEW_MAIN" > "$KONIX_SYNC_MAIN_DIR_FILE"
sed -i "${RES}d" "$KONIX_SYNC_DIR_FILE"
echo "This is the new push/pull list"
konix_sync_list
