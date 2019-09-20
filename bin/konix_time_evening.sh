#!/bin/bash

[ $(date '+%s') -gt $(date '+%s' -d '19:00') ] && [ $(date '+%s') -lt $(date '+%s' -d '21:00') ]
