#!/bin/bash
echo "poweroff de sur ? (o/n)"
read arg
if [[ $arg == "o" ]]
then
    poweroff
fi
