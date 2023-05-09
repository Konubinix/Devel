#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from datetime import timedelta

import click
from clk.core import cache_disk
from clk.decorators import argument, group, option
from clk.lib import echo_json
from clk.log import get_logger
from dateutil.parser import parse as parsedate

LOGGER = get_logger(__name__)


@cache_disk(expire=3600 * 24 * 150
            )  # about 1/2 year of validity, one year was too big
def fetch_holidays():
    url = "https://www.data.gouv.fr/fr/datasets/r/000ae493-9fa8-4088-9f53-76d375204036"
    LOGGER.info(f"Getting the data from {url}")
    import requests
    return requests.get(url).json()


@group()
def holidays():
    "Fetch and export holidays automatically"


@holidays.command()
def cat():
    """Dump the file as-is"""
    echo_json(fetch_holidays())


@holidays.command()
@argument("location", help="Location of interest")
@option("--population",
        help="Why kind of people are you interested in",
        type=click.Choice(["Élèves", "Enseignants"]),
        default="Élèves")
def export_emacs(location, population):
    "Export the holidays in a sexp emacs friendly format."
    data = fetch_holidays()
    print("(setq konix/school-holidays '(")
    for holiday in data:
        fields = holiday["fields"]
        if fields["location"] == location and fields["population"] in (
                "-", population):
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
