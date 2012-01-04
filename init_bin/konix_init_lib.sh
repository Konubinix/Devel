#!/bin/bash

# ####################################################################################################
# Function needed to import my custom env
# ####################################################################################################
import_env () {
    FORCE=$1
    if [ -n "$FORCE" ]
    then
        unset KONIX_ENV_DONE
    fi
    ENV_CUSTOM_FILE="$2"
    TEMP_FILE=$(mktemp -t "temp_file_for_import_env.XXXX")
    rm -f "${TEMP_FILE}"
    python "${HOME}/init_bin/konix_get_env.py" "$ENV_CUSTOM_FILE" 2>/dev/null| python "${HOME}/init_bin/konix_quote_env_variables.py" > "${TEMP_FILE}"
    echo "" >> "${TEMP_FILE}"
    if [ -n "$IMPORT_ENV_DEBUG" ]
    then
        cat "${TEMP_FILE}"
    fi
    while read line
    do
        if [ -n "$line" ]
        then
            eval "export $line"
        fi
    done < "${TEMP_FILE}"
    rm "${TEMP_FILE}"
}
