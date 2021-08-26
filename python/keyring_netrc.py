#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import json
import netrc
import os

import keyring.backend


class NetrcKeyring(keyring.backend.KeyringBackend):
    priority = 1

    def set_password(self, servicename, username, password):
        raise NotImplementedError

    def get_password(self, servicename, username):
        try:
            authenticator = netrc.netrc(
                os.path.expanduser("~/.netrc")).authenticators(username)
            return json.dumps((authenticator[0], authenticator[2]))
        except:
            return None

    def delete_password(self, servicename, username, password):
        raise NotImplementedError
