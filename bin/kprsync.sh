#!/bin/bash

rsync -az --info=progress2 "${@}"
