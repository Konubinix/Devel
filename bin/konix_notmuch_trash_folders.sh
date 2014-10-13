#!/bin/bash

folders="$(echo ${HOME}/Mail/*/\[Gmail\].{Trash,Bin,Corbeille})"
for folder in ${folders}
do
    echo "${folder}"|sed "s|${HOME}/Mail/||"
done
