#!/bin/bash -eux

blob="$(git fsck 2>&1 |grep "is empty"|head -1|sed -r 's|error: object file (.+) is empty|\1|')"
while [ -n "$blob" ]
do
    read -p "Remove ${blob}?"
    rm -f "${blob}"
    blob="$(git fsck 2>&1 |grep "is empty"|head -1|sed -r 's|error: object file (.+) is empty|\1|')"
done
