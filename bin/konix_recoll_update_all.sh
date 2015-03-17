#!/bin/bash

for recoll_dir in "${KONIX_PERSO_DIRS}"/*/recoll
do
    echo "#################"
    echo "Updating ${recoll_dir}"
    echo "#################"
    recollindex -c "${recoll_dir}"
done
