#!/bin/bash -eu

usage () {
    cat<<EOF
$0

Restart the kind cluster and run the appropriate post actions (taken from
https://github.com/kubernetes-sigs/kind/issues/148)
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
      [[ $(docker inspect -f '{{.State.Running}}' $container) == "true" ]] || docker start $container
done
sleep 1

docker exec ${CLK___CLUSTER_NAME}-control-plane sh -c 'mount -o remount,ro /sys; kill -USR1 1'
kubectl config set clusters.${KIND_CTX}.server $(kind get kubeconfig --name ${CLK___CLUSTER_NAME} -q | yq -r '.clusters[].cluster.server')
kubectl config set clusters.${KIND_CTX}.certificate-authority-data $(kind get kubeconfig --name ${CLK___CLUSTER_NAME} -q | yq -r '.clusters[].cluster."certificate-authority-data"')
kubectl config set users.${KIND_CTX}.client-certificate-data $(kind get kubeconfig --name ${CLK___CLUSTER_NAME} -q | yq -r '.users[].user."client-certificate-data"')
kubectl config set users.${KIND_CTX}.client-key-data $(kind get kubeconfig --name ${CLK___CLUSTER_NAME} -q | yq -r '.users[].user."client-key-data"')
