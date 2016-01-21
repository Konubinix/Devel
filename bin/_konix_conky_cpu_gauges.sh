#!/bin/bash

cat /proc/cpuinfo |grep processor|sed -r 's|processor.+: ||'|while read number
do
    n=$((number + 1))
    echo  "\$alignc$color3 CPU${n} \$color: \${outlinecolor black}\${cpu cpu${n}}% / \${freq 0}MHz
\${cpugraph cpu${n}}\${outlinecolor}"
done
