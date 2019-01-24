#!/bin/bash

[ $(date '+%s') -gt $(date '+%s' -d '18:30') ] && [ $(date '+%s') -lt $(date '+%s' -d '21:00') ]
