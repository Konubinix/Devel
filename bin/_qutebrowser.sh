#!/bin/bash -eu

WAIT_TIME=0.5

comm_redis_variable_name ( ) {
    basename "${QUTE_FIFO}"
}

comm_init () {
    local variable="$(comm_redis_variable_name)"
    cat <<EOF | send_js_multiline
qb_init = function () {
  var xhr = new XMLHttpRequest();
  function processRequest(e) {
    if (xhr.readyState == 4)
    {
      if (xhr.status == 200)
      {
        // ok
      }
      else
      {
         // var response = JSON.parse(xhr.responseText);
        alert(xhr.responseText);
      }
    }
  };
  xhr.onreadystatechange = processRequest;
return xhr;
};

document.qb_lpush = function (arg) {
xhr = qb_init();
  xhr.open('GET', "http://127.0.0.1:7379/LPUSH/${variable}/" + arg, true);
  xhr.send();
};
document.qb_done = function (arg) {
  xhr.open('GET', "http://127.0.0.1:7379/LPUSH/${variable}_sync/" + arg, true);
  xhr.send();
};
EOF
}

send_js_multine_sync () {
    send_js_multiline
    comm_redis_bpop
}

send_js_sync () {
    send_js "$*"
    comm_redis_bpop
}

send_js_sync_get () {
    send_js "$document.qb_lpush($*);"
    comm_redis_bpop
}

comm_redis_send () {
    local expr="$1"
    send_js "document.qb_lpush(${expr});"
}

comm_test_redis_send () {
    local value="$1"
    send_js "document.qb_lpush('${value}');"
}

comm_redis_wait () {
    curl "http://127.0.0.1:7379/BRPOP/$(comm_redis_variable_name)_sync/${timeout}" 2> /dev/null |jq -r '.BRPOP[1]'
}

comm_redis_flush () {
    curl "http://127.0.0.1:7379/BRPOP/$(comm_redis_variable_name)_sync/${timeout}" 2> /dev/null |jq -r '.BRPOP[1]'
}

comm_redis_pop () {
    curl "http://127.0.0.1:7379/LPOP/$(comm_redis_variable_name)" 2> /dev/null |jq -r .LPOP
}

comm_redis_bpop () {
    local timeout="${1:-0}"
    curl "http://127.0.0.1:7379/BRPOP/$(comm_redis_variable_name)/${timeout}" 2> /dev/null |jq -r '.BRPOP[1]'
}

format_js () {
    sed 's,^ *//.*$,,' | tr '\n' ' '
}

send_command_multiline ( ) {
    cat >> "$QUTE_FIFO"
    sleep "${WAIT_TIME}"
}

send_command_multiline_fast ( ) {
    cat >> "$QUTE_FIFO"
}

send_command ( ) {
    echo "$*"|send_command_multiline
}

send_command_fast ( ) {
    echo "$*"|send_command_multiline_fast
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

send_tab ( ) {
    local number="${1:-1}"
    {
        for _ in $(seq "${number}")
        do
            echo "fake-key <TAB>"
        done
    } | send_command_multiline
}

send_return ( ) {
    local number="${1:-1}"
    {
        for _ in $(seq "${number}")
        do
            echo "fake-key <return>"
        done
    } | send_command_multiline
}

comm_init
