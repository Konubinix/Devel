#!/bin/bash

# taken from http://stackoverflow.com/questions/2564634/bash-convert-absolute-path-into-relative-path-given-a-current-directory
# Test cases :
#
# $0 "/A/B/C" "/A"           -->  "../.."
# $0 "/A/B/C" "/A/B"         -->  ".."
# $0 "/A/B/C" "/A/B/C"       -->  ""
# $0 "/A/B/C" "/A/B/C/D"     -->  "D"
# $0 "/A/B/C" "/A/B/C/D/E"   -->  "D/E"
# $0 "/A/B/C" "/A/B/D"       -->  "../D"
# $0 "/A/B/C" "/A/B/D/E"     -->  "../D/E"
# $0 "/A/B/C" "/A/D"         -->  "../../D"
# $0 "/A/B/C" "/A/D/E"       -->  "../../D/E"
# $0 "/A/B/C" "/D/E/F"       -->  "../../../D/E/F"
# both $1 and $2 are absolute paths beginning with /
# returns relative path to $2/$target from $1/$source
source="$1"
target="$2"

common_part=$source # for now
result="" # for now

while [[ "${target#$common_part}" == "${target}" ]]; do
    # no match, means that candidate common part is not correct
    # go up one level (reduce common part)
    common_part="$(dirname $common_part)"
    # and record that we went back, with correct / handling
    if [[ -z $result ]]; then
        result=".."
    else
        result="../$result"
    fi
done

if [[ $common_part == "/" ]]; then
    # special case for root (no common path)
    result="$result/"
fi

# since we now have identified the common part,
# compute the non-common part
forward_part="${target#$common_part}"

# and now stick all parts together
if [[ -n $result ]] && [[ -n $forward_part ]]; then
    result="$result$forward_part"
elif [[ -n $forward_part ]]; then
    # extra slash removal
    result="${forward_part:1}"
fi

echo $result
