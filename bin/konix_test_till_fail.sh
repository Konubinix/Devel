#!/bin/bash

a=0
TEST_LOG="test_log"
> "$TEST_LOG"
while $* >> "$TEST_LOG"
do a=$((a+1))
    printf '\b\b\b\b\b%s' "$a"
done
