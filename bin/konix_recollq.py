#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import konix_recolllib

def main():
    konix_recolllib.call_recoll("recoll", ["-t", "-q"])

if __name__ == "__main__":
    main()
