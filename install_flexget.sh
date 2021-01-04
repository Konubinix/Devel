#!/bin/bash

ME="$(basename "BASH_SOURCE[0]")"
pushd "Flexget"
{
	python3 setup.py develop --user
}
popd
