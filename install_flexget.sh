#!/bin/bash

DIR="$(dirname "$(basename "BASH_SOURCE[0]")")"
pushd "${DIR}/Flexget"
{
	python3 setup.py develop --user
}
popd
