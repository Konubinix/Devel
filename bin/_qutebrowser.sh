#!/bin/bash -eu

format_js () {
    sed 's,//.*$,,' | tr '\n' ' '
}

send_command_multiline ( ) {
    echo "$(cat)" >> "$QUTE_FIFO"
    sleep 0.5
}

send_command ( ) {
    echo "$*"|send_command_multiline
}

remove_text ( ) {
    echo "fake-key <ctrl-a>" | send_command_multiline
    echo "fake-key <DEL>" | send_command_multiline
}

focus_on ( ) {
    send_js "$*.focus();"
}

focus_on_id ( ) {
    local id="$1"
    focus_on "document.getElementById('${id}')"
}

click_on ( ) {
    send_js "$*.click();"
}

click_on_id ( ) {
    local id="$1"
    click_on "document.getElementById('${id}')"
}

replace_text_multiline () {
    remove_text
    echo "insert-text $(cat)" | send_command_multiline
}

replace_text ( ) {
    echo "$*" | replace_text_multiline
}

replace_text_on ( ) {
    local elem="$1"
    local content="$2"
    focus_on "${elem}"
    replace_text "${content}"
}

replace_text_on_id ( ) {
    local id="$1"
    local content="$2"
    focus_on_id "${id}"
    replace_text "${content}"
}

replace_with_fake_keys_text_multiline () {
    remove_text
    echo "fake-key $(cat)" | send_command_multiline
}

replace_with_fake_keys_text ( ) {
    echo "$*" | replace_with_fake_keys_text_multiline
}

replace_with_fake_keys_text_on ( ) {
    local elem="$1"
    local content="$2"
    focus_on "${elem}"
    replace_with_fake_keys_text "${content}"
}

replace_with_fake_keys_text_on_id ( ) {
    local id="$1"
    local content="$2"
    focus_on_id "${id}"
    replace_with_fake_keys_text "${content}"
}

say_multiline ( ) {
    konix_display.py "$(cat)"
}

say ( ) {
    echo "message-info '$*'" | send_command_multiline
}

send_js ( ) {
    echo "$*" | send_js_multiline
}

send_js_multiline ( ) {
    echo "jseval -q $(format_js)" | send_command_multiline
}
