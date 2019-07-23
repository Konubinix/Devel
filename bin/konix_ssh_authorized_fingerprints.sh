#!/bin/bash -u

while read l; do
    [[ -n $l && ${l###} = $l ]] && {
        ssh-keygen -l -E md5 -f /dev/stdin <<<$l || {
            # hack for old ssh install
            echo $l|sed -r 's/.*ssh-rsa [^ ]+ (.+)$/\1/'
            ssh-keygen -l -f /dev/stdin <<<$l
        }
    } 2>/dev/null
    echo "-----"
done < "${HOME}/.ssh/authorized_keys"
