#!/bin/bash -eu

usage () {
    cat<<EOF
$0

Run k3d the way I like it
--
F:--metallb/--no-metallb:Also install metallb:True
F:--registry/--no-registry:Also connect to local registry:True
F:--debug/--no-debug:Enable set -x
N:Remaining args to give to k3d cluster create
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

args=(
	--k3s-server-arg --no-deploy=traefik \
					 --port 443:443@loadbalancer \
					 --port 80:80@loadbalancer "$@"
)

if [ "${CLK___DEBUG}" == "True" ]
then
	set -x
fi

if [ "${CLK___REGISTRY}" == "True" ]
then
	# https://k3d.io/usage/guides/registries/
	mkdir -p "${HOME}/.k3d"
	cat <<EOF > "${HOME}/.k3d/my-registries.yaml"
mirrors:
  "registry.localhost:5000":
    endpoint:
      - "http://registry.localhost:5000"
EOF
	args+=(
		--volume "${HOME}/.k3d/my-registries.yaml:/etc/rancher/k3s/registries.yaml"
	)
	if ! docker volume ls --format '{{ .Name }}'|grep -q '^local_registry$'
	then
		clk log -l info "Creating the local registry volume"
		docker volume create local_registry
	fi
	if ! docker ps -a --format '{{.Names}}'|grep -q '^registry.localhost$'
	then
		clk log -l info "Creating localhost docker registry"
		docker container run -d --name registry.localhost -v local_registry:/var/lib/registry --restart always -p 5000:5000 registry:2
	else
		clk log -l info "Starting localhost docker registry"
		docker start registry.localhost
	fi

fi

k3d cluster create "${args[@]}"

if [ "${CLK___REGISTRY}" == "True" ] && [ "null" == "$(docker inspect k3d-k3s-default|jq '.[].Containers | map(select(.Name=="registry.localhost"))[0]')" ]
then
	clk log -l info "Connecting the k3d instance to the local registry"
	docker network connect k3d-k3s-default registry.localhost
fi

if [ "${CLK___METALLB}" == "True" ]
then
	clk k3d install-metallb@sh
fi
