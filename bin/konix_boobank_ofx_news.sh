#!/bin/bash

for i in ofx_*
do
	echo "Importing in $i"
	pushd "$i"
	konix_boobank_ofx_new.sh
	popd
done
