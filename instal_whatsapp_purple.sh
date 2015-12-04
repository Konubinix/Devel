#!/bin/bash

set -eu
cd whatsapp-purple
make
mv libwhatsapp.so "${HOME}/.purple/plugins/"
