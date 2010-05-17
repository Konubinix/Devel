#!/bin/bash
echo "reboot de sur ? (o/n)"
read arg
if [[ $arg == "o" ]]
then
    reboot
fi
