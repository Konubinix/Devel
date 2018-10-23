#!/bin/bash

git annex find --print0 --metadata ack=restored \
	| xargs -0 -n 1 ls -lNd
