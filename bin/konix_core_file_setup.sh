#!/bin/bash

echo '/tmp/core/%e_%t_%p_%u_%s_%c' > /proc/sys/kernel/core_pattern
