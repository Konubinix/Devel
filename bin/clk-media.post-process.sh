#!/bin/bash
if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

clk images post-process@sh
clk videos post-process@sh
