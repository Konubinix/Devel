#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import requests
from clk.decorators import command, argument, option, flag


@command()
@option("--url", help="The url of the gotify server, including the token")
@option("--priority", "-p", type=int, help="1 to 5", default=2)
@flag("--verify/--no-verify", help="Verify the SSL certificate")
@argument("title", help="The title")
@argument("body", help="The body")
def gotify(url, priority, title, body, verify):
    "Push some notifications to gotify"
    requests.post(
        url,
        json={
            "message": body,
            "priority": priority,
            "title": title,
        },
        verify=verify,
    )
