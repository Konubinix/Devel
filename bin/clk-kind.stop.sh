#!/bin/bash -eu

usage () {
    cat<<EOF
$0

Stop the kind cluster
--
O:-n,--cluster-name:str:The name of the cluster:kind
F:-d,--debug:Debug mode
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

if [ "${CLK___DEBUG}" == "True" ]
then
    set -x
fi

KIND_CTX="kind-${CLK___CLUSTER_NAME}"

for container in $(kind get nodes --name ${CLK___CLUSTER_NAME}); do
      [[ $(docker inspect -f '{{.State.Running}}' $container) != "true" ]] || docker stop $container
done
