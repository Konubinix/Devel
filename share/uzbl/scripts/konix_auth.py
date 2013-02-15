#!/usr/bin/env python
# -*- coding:utf-8 -*-

import subprocess
import os
import netrc
import sys
import auth                     # local import

if __name__ == "__main__":
    # $1 authentication zone unique identifier (may be used as 'key')
    # $2 domain part of URL that requests authentication
    # $3 authentication realm
    # $4 FALSE if this is the first attempt to authenticate, TRUE otherwise
    net = netrc.netrc()
    authenticators = net.authenticators(sys.argv[2])
    if authenticators:
        (login, account, password) = authenticators
        print login
        print password
    else:
        rv, output = auth.getText(sys.argv[1], sys.argv[2], sys.argv[3])
        if (rv == gtk.RESPONSE_OK):
            print output;
        else:
            exit(1)
