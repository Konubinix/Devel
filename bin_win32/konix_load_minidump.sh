#!/bin/bash

windbg -z "$1" -y SRV*c:\symbols*http://msdl.microsoft.com/download/symbols
