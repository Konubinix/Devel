#!/bin/bash

echo "js page string 'var konix_edited_elem = document.activeElement;var konix_edited_elem_old_background_color = konix_edited_elem.style.backgroundColor;konix_edited_elem.style.backgroundColor=\"lightBlue\";'" > "$UZBL_FIFO"
