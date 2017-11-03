#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import json, pprint, sys

content = json.loads(open(sys.argv[1], "rb").read().decode("utf-8"))
content = json.dumps(content, indent=4, sort_keys=True).replace(' \n', '\n') + '\n'
open(sys.argv[1], "wb").write(content.encode("utf-8"))
