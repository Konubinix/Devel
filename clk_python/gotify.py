#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import requests
from clk.decorators import argument, command, flag, option


@command()
@option("--url", help="The url of the gotify server, including the token")
@option("--priority", "-p", type=int, help="1 to 5", default=2)
@flag("--verify/--no-verify", help="Verify the SSL certificate")
@argument("title", help="The title")
@argument("body", help="The body")
def gotify(url, priority, title, body, verify):
    "Push some notifications to gotify"
    if not verify:
        # I know what I am doing
        import urllib3
        urllib3.disable_warnings()
    requests.post(
        url,
        json={
            "message": body,
            "priority": priority,
            "title": title,
        },
        verify=verify,
    )
