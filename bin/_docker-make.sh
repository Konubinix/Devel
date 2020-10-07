#!/bin/bash -eu

docker-make-options () {
    echo "A:name:$(docker-make-targets|list_to_choice):Name of the image to build"
    echo 'O:--platform:["linux/amd64", "linux/arm/v7", "linux/amd64,linux/386,linux/arm/v7", "linux/amd64,linux/386", "linux/arm/v6"]:Platform to build to:linux/amd64,linux/386,linux/arm/v7'
    echo 'N:Arguments to pass to docker buildx build as-is'
    echo "O:-t,--tag:str:Tag:latest"
    echo "O:--build-args:str:Build args"
}

_docker-make-targets () {
    docker-make -l|grep '^ \*'|sed 's/^ \* //'
}

docker-make-targets () {
    _docker-make-targets
}

docker-make-build-args () {
    args=()
    if [ "${CLK___BUILD_ARGS}" != "" ]
    then
        {
            OLDIFS="${IFS}"
            IFS=","
            for arg in "${CLK___BUILD_ARGS}"
            do
                args+=("--build-arg" "${arg}")
            done
            IFS="${OLDIFS}"
        }
    fi
}

docker-make-it () {
    docker-make -pn "${CLK___NAME}"
    docker buildx build \
           "${args[@]}" \
           -t "localhost:9692/${CLK___NAME}:${CLK___TAG}" \
           --platform "${CLK___PLATFORM}" \
		   --push \
           "." \
           -f "./docker_makefiles/Dockerfile.${CLK___NAME}" \
           ${CLK___ARGS}
}
