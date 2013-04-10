#!/bin/bash
# inspired from a work from ThArGos

export LC_ALL=C
for package in `aptitude -O installsize search ~i | tac | cut -c 5- | cut -d ' ' -f 1`
do
    aptitude show "${package}" | sed -n "
/^Uncompressed Size:/ {
  s/^Uncompressed Size: \+\(.\+\)\$/\1\t\t${package}/ p
}
"

done
