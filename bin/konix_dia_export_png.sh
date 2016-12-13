#!/bin/bash

DIA="${1}"
PNG="${DIA%%.dia}.png"

dia -e "${PNG}" "${DIA}"
