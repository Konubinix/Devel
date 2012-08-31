#/bin/bash

# Author: Jurica Bradaric <jbradaric at gmail.com>
# A simple script to use custom stylesheets for websites.
# Similar to the Stylish extension for Firefox.
# I hope someone finds it useful. All comments and suggestions
# are welcome.

# Set the path to the .css files
# Every custom css style must be in a file named web_address.css
# e.g. reddit.com.css
UZBL_CONFIG=$XDG_CONFIG_HOME/uzbl
STYLE_PATH=$UZBL_CONFIG/css

STYLESHEET_SET=0

for i in $STYLE_PATH/*.css; do
    stylesheet=$(basename $i '.css')
    if echo "$UZBL_URI" | grep -q "${stylesheet}"
    then
        echo "set stylesheet_uri = file://${STYLE_PATH}/${stylesheet}.css" > "$UZBL_FIFO"
        STYLESHEET_SET=1
        break
    fi
done

exit 0
