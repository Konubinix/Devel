#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sys
import os
import argparse

import jnius

parser = argparse.ArgumentParser(
    description="""Export a secret from a keystore.

    For now, exporting the keystore into pkcs12 and reading the secret with
    openssl raises a warning "Warning unsupported bag type: secretBag" and won't
    show the secret.

    Using the code from https://stackoverflow.com/a/36484915
"""
)

parser.add_argument('keystore',
                    help='the key store')
parser.add_argument('keystore_pass',
                    help='the password of the key store')
parser.add_argument("-t", '--store-type',
                    help='the store type', default="jceks")
parser.add_argument("-a", '--alias',
                    help='the alias of the secret in the store', required=False)
parser.add_argument("-p", '--secret-pass',
                    help='the password of the secret to extract', required=False)


def main():
    args = parser.parse_args()
    if not args.alias:
        print("Don't know the alias names ?")
        cmd = (
            "keytool -list "
            "-keystore '{}' "
            "-storepass '{}' "
            "-storetype '{}'"
        ).format(
            args.keystore,
            args.keystore_pass,
            args.store_type
        )
        print(cmd)
        os.system(cmd)
        exit(0)
    if not args.secret_pass:
        args.secret_pass = args.keystore_pass
    KeyStore = jnius.autoclass("java.security.KeyStore")
    FileInputStream = jnius.autoclass("java.io.FileInputStream")
    File = jnius.autoclass("java.io.File")

    ks = KeyStore.getInstance(args.store_type.upper())
    ks.load(FileInputStream(File(args.keystore)), args.keystore_pass)
    key = ks.getKey(args.alias, args.secret_pass)
    value = key.getEncoded().tostring()
    sys.stdout.write(value)


if __name__ == "__main__":
    main()
