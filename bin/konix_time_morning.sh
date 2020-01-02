#!/bin/bash

[ $(date '+%s') -gt $(date '+%s' -d '5:00') ] && [ $(date '+%s') -lt $(date '+%s' -d '10:00') ]
