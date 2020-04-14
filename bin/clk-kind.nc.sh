#!/bin/bash -eu

list_svc () {
    kubectl get svc -o json |jq '.items[].metadata.name'|paste -s - -d','
}

usage () {
    cat<<EOF
$0

Ge the services.
--
A:service:[$(list_svc)]:The service to scan
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi


nc $(clk kind service-ip@sh "${CLK___SERVICE}")
