#!/bin/bash

BAT=BAT0
if ! [ -e "/sys/class/power_supply/${BAT}" ]
then
    BAT=battery
fi
if ! [ -e "/sys/class/power_supply/${BAT}" ]
then
    exit 1
fi
echo "\${outlinecolor}\${color2}Battery status :\$color\${outlinecolor black}\$alignc\${battery_time ${BAT}}
\${battery_bar ${BAT}}${outlinecolor}"
