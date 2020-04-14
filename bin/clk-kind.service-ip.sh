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

CONTROL_PLANE_IP="$(docker inspect kind-control-plane | jq -r '.[].NetworkSettings.Networks.bridge.IPAddress')"
NODE_PORT="$(kubectl get svc "${CLK___SERVICE}" -o=json | jq '.spec.ports[].nodePort')"
echo "${CONTROL_PLANE_IP} ${NODE_PORT}"
