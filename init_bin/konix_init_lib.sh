#!/bin/bash

# put the python of this platform in front of the path
# the only hacky stuf I need is the platform
export WANTED_PLATFORM="$(source "${HOME}/init_bin/_konix_platform.sh")"
PATH_SEPARATOR="$(cd "${HOME}/init_bin" && "./_konix_get_default_env.py" PATH_SEPARATOR)"
export PYTHON_BIN="$(cd "${HOME}/init_bin" && "./_konix_get_default_env.py" PYTHON_BIN)"
export PYTHON_PATH="$(cd "${HOME}/init_bin" && ./konix_dirname.py "$PYTHON_BIN")"
# now, I am sure the python bin is set before running the init_lib
# Import env variables

# ####################################################################################################
# Function needed to import my custom env
# ####################################################################################################
import_env () {
    OLD_PWD="${PWD}"
    ENV_CUSTOM_FILE="${1:-}"
    EXPORT_KEYWORD="${2:-export}"
    TEMP_FILE=$(mktemp -t "temp_file_for_import_env.XXXX")
    rm -f "${TEMP_FILE}"
    "$PYTHON_BIN" "${HOME}/init_bin/konix_get_env.py" "$ENV_CUSTOM_FILE" 2>/dev/null| "$PYTHON_BIN" "${HOME}/init_bin/konix_quote_env_variables.py" > "${TEMP_FILE}"
    echo "" >> "${TEMP_FILE}"
    if [ -n "${IMPORT_ENV_DEBUG:-}" ]
    then
        cat "${TEMP_FILE}"
    fi
    while read line
    do
        if [ -n "$line" ]
        then
            if echo "$line" |egrep -q "=''\$"
            then
                eval "unset $(echo "$line"|cut -f1 -d"=")"
            else
                if ! eval "${EXPORT_KEYWORD} $line"
				then
					echo "Failure when evaluating line '${line}'" >&2
				fi
            fi
        fi
    done < "${TEMP_FILE}"
    # restore the old pwd
    PWD="${OLD_PWD}"
    rm "${TEMP_FILE}"
}
