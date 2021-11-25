#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from datetime import timedelta

from clk.core import cache_disk
from clk.decorators import argument, group
from clk.log import get_logger
from dateutil.parser import parse as parsedate

LOGGER = get_logger(__name__)


@cache_disk(expire=3600 * 24 * 300)  # about one year of validity
def fetch_holidays():
    url = "https://www.data.gouv.fr/fr/datasets/r/000ae493-9fa8-4088-9f53-76d375204036"
    LOGGER.info(f"Getting the data from {url}")
    import requests
    return requests.get(url).json()


@group()
def holidays():
    "Fetch and export holidays automatically"


@holidays.command()
@argument("location", help="Location of interest")
def export_emacs(location):
    "Export the holidays in a sexp emacs friendly format."
    data = fetch_holidays()
    print("(setq konix/school-holidays '(")
    for holiday in data:
        fields = holiday["fields"]
        if fields["location"] == location:
            oneday = timedelta(days=1)
            start_date = parsedate(fields["start_date"])
            end_date = parsedate(fields["end_date"])
            current_date = start_date
            while (current_date.strftime("%Y-%m-%d") !=
                   end_date.strftime("%Y-%m-%d")):
                print(
                    f"""(({current_date.month} {current_date.day} {current_date.year}) "{fields["description"]}")"""
                )
                current_date += oneday
    print("))")
