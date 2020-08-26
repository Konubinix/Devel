#!/usr/bin/env python
# -*- coding:utf-8 -*-

import json
import os
import logging
import subprocess
import six
from contextlib import contextmanager

import keyring.backend


@contextmanager
def updated_env(**kwargs):
    u"""Temporarily update the environment. To be used in a with statement"""
    oldenv = dict(os.environ)
    for k, v in six.iteritems(kwargs):
        if v is None and k in os.environ:
            del os.environ[k]
        else:
            os.environ[k] = v
    yield
    os.environ.clear()
    os.environ.update(oldenv)


class ImpassKeyring(keyring.backend.KeyringBackend):
    priority = 1

    def set_password(self, servicename, username, password):
        raise NotImplementedError

    def get_password(self, servicename, username):
        key = "{}@{}".format(username, servicename)
        try:
            with updated_env(IMPASS_DUMP_PASSWORDS="1"):
                return json.loads(
                    subprocess.check_output(
                        [
                            "impass",
                            "dump",
                            key,
                        ]
                    ).decode("utf-8").strip()
                )[key]["password"]
        except KeyError:
            logging.error(f"Could not find password for {key}")
            return None

    def delete_password(self, servicename, username, password):
        raise NotImplementedError
