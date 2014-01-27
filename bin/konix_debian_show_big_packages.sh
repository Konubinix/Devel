#!/bin/bash
# inspired from a work from ThArGos

export LC_ALL=C
for package in `aptitude -O installsize search ~i | tac | cut -c 5- | cut -d ' ' -f 1`
do
    echo -e "`aptitude reinstall -sy ${package} | grep archive | sed -e "s@.*/\(.*\)B of archives.*@\1@"` \t${package}"
done
