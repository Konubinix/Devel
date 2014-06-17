#!/bin/bash

# if the repository is not bare, make is not bare explicitly, or else
# synchronization will remove my files
if ! git config core.bare > /dev/null
then
    git config core.bare false
fi
