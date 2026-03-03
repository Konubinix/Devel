#!/usr/bin/env bash

impass add "$@" || impass replace "$@"
