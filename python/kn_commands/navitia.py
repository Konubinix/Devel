#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from __future__ import print_function, absolute_import

import time
import os
import re
import datetime
import builtins
import json
from dateutil.parser import parse as parsetime
import pprint
import asyncio
import base64
from requests.exceptions import HTTPError
import requests
from dataclasses import dataclass, field
import requests

from bs4 import BeautifulSoup as soup
import asyncio_redis
import websockets
import click
import parsedatetime

from click_project.decorators import (
    group,
    table_fields,
    table_format,
    argument,
    param_config,
    option,
    flag,
)
from click_project.log import get_logger
from click_project.lib import (
    get_authenticator,
    TablePrinter,
    ParameterType,
    makedirs
)
from click_project.completion import startswith
from click_project.config import config
from click_project.core import cache_disk

from konix_time_helper import naive_to_local

LOGGER = get_logger(__name__)


class AttrDict(dict):
    def __init__(self, *args, **kwargs):
        super(AttrDict, self).__init__(*args, **kwargs)
        self.__dict__ = self


def get_navitia_key():
    values = get_authenticator(f"navitia_{config.navitia.coverage}_key", required=False, askpass=False)
    if values is not None:
        return values[1]
    else:
        return None


class HasVehicleJourneyMixin:
    @property
    def vehicle_journey_id(self):
        id = [
            l for l in self.links
            if l['type'] == "vehicle_journey"
        ][0]["id"]
        return id

    @property
    def vehicle_journey(self):
        x = config.navitia.get(f"vehicle_journeys/{self.vehicle_journey_id}").json()["vehicle_journeys"][0]
        return VehicleJourney(**x)

    def format_impact(self):
        return self.vehicle_journey.format_impact()


@dataclass
class PTObject:
    embedded_type: str
    quality: int
    name: str
    id: str
    stop_area: dict = None
    stop_point: dict = None
    administrative_region: dict = None
    line: dict = None

    def __post_init__(self):
        if self.stop_area:
            self.stop_area = StopArea(**self.stop_area)
        if self.stop_point:
            self.stop_point = StopPoint(**self.stop_point)
        if self.stop_point and self.stop_area is None:
            self.stop_area = self.stop_point.stop_area


@dataclass
class Route:
    direction: dict
    name: str
    links: list
    direction_type: str
    geojson: dict
    id: str
    is_frequence: bool
    line: dict = None
    physical_modes: list = None

    def __post_init__(self):
        self.direction = PTObject(**self.direction)
        if self.line:
            self.line = Line(**self.line)

    @property
    def schedules(self):
        return [
            RouteSchedule(**info)
            for info in config.navitia.get(f"routes/{self.id}/route_schedules").json()["route_schedules"]
        ]


@dataclass
class Line:
    code:str
    name:str
    links:list
    color:str
    geojson:dict
    text_color:str
    codes:list
    closing_time:str
    opening_time:str
    id:str
    network: list = None
    routes: list = None
    physical_modes: list = None
    commercial_mode: list = None

    def __post_init__(self):
        if self.routes is not None:
            self.routes = [Route(**route) for route in self.routes]

    @property
    def route_schedules(self):
        return [
            RouteSchedule(**info)
            for info in config.navitia.get(f"lines/{self.id}/route_schedules").json()["route_schedules"]
        ]


@dataclass
class RouteSchedule:
    display_informations:str
    table:str
    additional_informations:str
    geojson:str
    links:str

    def __post_init__(self):
        self.rows = [Row(**item) for item in self.table["rows"]]


@dataclass
class Row:
    stop_point:str
    date_times:str

    def __post_init__(self):
        self.stop_point = StopPoint(**self.stop_point)
        self.dts = [
            ScheduleDateTime(**dt)
            for dt in self.date_times
        ]


@dataclass
class ScheduleDateTime(HasVehicleJourneyMixin):
    date_time:str
    additional_informations:str
    data_freshness:str
    links:str
    base_date_time:str = ""


@dataclass
class StopScheduleDateTime(ScheduleDateTime):
    schedule: str = ""

    @property
    def route_schedule(self):
        return self.vehicle_journey.route_schedule

    @property
    def disrupted_route(self):
        return self.vehicle_journey.disruption.impacted_stops


@dataclass
class Disruption:
    status: str
    disruption_id: str
    severity: str
    impact_id: str
    application_periods: str
    updated_at: str
    uri: str
    impacted_objects: str
    disruption_uri: str
    contributor: str
    cause: str
    id: str
    messages: str = ""

    def __post_init__(self):
        self.impacted_stops = self.impacted_objects[0]["impacted_stops"]
        for stop in self.impacted_stops:
            stop["stop_point"] = StopPoint(**stop["stop_point"])
        self.impacted_pt_object = self.impacted_objects[0]["pt_object"]

    def format_impact(self):
        res = ""
        for i in self.impacted_stops:
            trainstation_info = i['stop_point'].trainstation_info.get(self.impacted_pt_object["trip"]["name"], {})
            res += f"""
{i['stop_point'].name}
  quay: {trainstation_info.get("quay", "N/A")}, prob: {trainstation_info.get("problem", "N/A")}
  cause: {i['cause']}
  {i['base_arrival_time']} -> {i['base_departure_time']}
  {i.get('amended_arrival_time', 'never')} -> {i.get('amended_departure_time', 'never')} ({i['arrival_status']}, {i['departure_status']})
"""
        return res


@dataclass
class VehicleJourney:
    codes:str
    name:str
    journey_pattern:str
    stop_times:str
    validity_pattern:str
    id:str
    trip:str
    disruptions:str
    calendars:str = None

    def format_impact(self):
        res = self.id + "\n"
        res += self.disruption.format_impact()
        return res

    @property
    def disruption(self):
        try:
            return next(self.disruptions_)
        except StopIteration:
            return None

    @property
    def stop_points(self):
        return [StopPoint(**time["stop_point"]) for time in self.stop_times]

    @property
    def disruptions_(self):
        return (
            Disruption(**config.navitia.get(f"disruptions/{d['id']}").json()["disruptions"][0])
            for d in self.disruptions
        )

    @property
    def route_schedule(self):
        rs = config.navitia.get(f"vehicle_journeys/{self.id}/route_schedules").json()["route_schedules"]
        assert len(rs) == 1
        return RouteSchedule(**rs[0])


@dataclass
class StopPoint:
    name:str
    links:str
    coord:str
    label:str
    equipments:str
    id:str
    stop_area:str = None
    commercial_modes:str = None
    administrative_regions:str = None
    physical_modes:str = None
    zip_codes_to_regions: str = field(default_factory=lambda: {
        "01": "auvergne-rhone-alpes",
        "38": "auvergne-rhone-alpes",
        "69": "auvergne-rhone-alpes",
        "42": "auvergne-rhone-alpes",
        "83": "paca",
    })

    @property
    def region(self):
        if self.administrative_regions is None:
            self.administrative_regions = config.navitia.get(f"stop_points/{self.id}").json()["stop_points"][0]["administrative_regions"]
        return self.zip_codes_to_regions[self.administrative_regions[0]["zip_code"][:2]]

    @property
    def trainstation_info(self):
        match = re.match("^stop_point:OCE:SP:.+-(.+)$", self.id)
        if match is None:
            return {}

        @cache_disk(expire=300)
        def _trainstation_info(id, name, region):
            url = f"https://www.ter.sncf.com/{region}/gares/{id}/{name}/prochains-departs"
            LOGGER.info(f"Getting {url}")
            r = requests.get(url)
            s = soup(r.content, "lxml")
            res = {}
            rows = s.find(attrs={"class":"train_depart_table"}).find("tbody")
            for row in rows.find_all("tr"):
                if row.get("class") == ["problem"]:
                    res[number.text.strip()]["problem"] = row.text.strip()
                    continue
                hour, destination, glass, number, mode, quay = row.find_all("td")
                res[number.text.strip()] = {
                    "hour": hour.text.strip(),
                    "destination": destination.text.strip(),
                    "number": number.text.strip(),
                    "mode": mode.text.strip(),
                    "quay": quay.text.strip(),
                }
            return res

        return _trainstation_info(match.group(1), self.name.split(" ")[0], self.region)

    @property
    def _stop_area(self):
        if self.stop_area is None:
            self.stop_area = config.navitia.get(f"stop_points/{self.id}/stop_areas").json()["stop_areas"][0]
        return StopArea(**self.stop_area)

    @property
    def schedules(self):
        return [
            StopSchedule(**item)
            for item in
            config.navitia.get(f"stop_areas/{self.id}/stop_schedules").json()["stop_schedules"]
        ]


@dataclass
class Stop(HasVehicleJourneyMixin):
    display_informations: str
    stop_point: dict
    route: dict
    links: list
    stop_date_time: dict

    @property
    def disruption(self):
        disruption = self.vehicle_journey.disruption
        if disruption is not None:
            return [
                impact for impact in disruption.impacted_stops
                if impact["stop_point"].id == self.stop_point.id
            ][0]
        return None

    @property
    def trainstation_info(self):
        return self.stop_point.trainstation_info.get(self.display_informations["headsign"], {})

    @property
    def quay(self):
        return self.trainstation_info.get("quay", "N/A")

    @property
    def problem(self):
        return self.trainstation_info.get("problem", "N/A")

    def format(self):
        res = f"""{self.display_informations.direction}
quay: {self.quay}, prob: {self.problem}, vehicle: {self.vehicle_journey.id}
{self.stop_date_time['base_arrival_date_time'].strftime("%m/%d %H:%M")} -> {self.stop_date_time['arrival_date_time'].strftime("%m/%d %H:%M")}
{self.stop_date_time['base_departure_date_time'].strftime("%m/%d %H:%M")} -> {self.stop_date_time['departure_date_time'].strftime("%m/%d %H:%M")}
"""
        disruption = self.disruption
        if disruption is not None:
            res += f"""Impact:
cause: {disruption["cause"]}
{disruption["base_arrival_time"]} -> {disruption["base_departure_time"]}
{disruption["amended_arrival_time"]} -> {disruption["amended_departure_time"]} ({disruption["arrival_status"]}, {disruption["stop_time_effect"]}, {disruption["departure_status"]})
"""
        return res

    def __post_init__(self):
        self.route = Route(**self.route)
        self.display_informations = AttrDict(self.display_informations)
        self.stop_point = StopPoint(**self.stop_point)
        for key in ("base_arrival_date_time", "base_departure_date_time", "arrival_date_time", "departure_date_time"):
            self.stop_date_time[key] = parsetime(self.stop_date_time[key])


@dataclass
class StopArea:
    codes: dict
    name: str
    links: list
    coord: dict
    label: str
    timezone: str
    id: str
    administrative_regions: list = None

    @property
    def stop_points(self):
        return [
            StopPoint(**sp)
            for sp in config.navitia.get(f"stop_areas/{self.id}/stop_points").json()["stop_points"]
        ]

    def departures_going_to(self, stop):
        if isinstance(stop, StopPoint):
            stop = stop.stop_area
        elif isinstance(stop, PTObject):
            stop = stop.stop_area
        res = []
        for departure in self.departures:
            found = False
            for stop_point in departure.vehicle_journey.stop_points:
                if stop_point._stop_area.id == self.id:
                    found = True
                if stop_point._stop_area.id == stop.id and found:
                    res.append(departure)
        return res

    def arrivals_coming_from(self, stop):
        if isinstance(stop, StopPoint):
            stop = stop.stop_area
        elif isinstance(stop, PTObject):
            stop = stop.stop_area
        res = []
        for arrival in self.arrivals:
            found = False
            for stop_point in reversed(arrival.vehicle_journey.stop_points):
                if stop_point._stop_area.id == self.id:
                    found = True
                if stop_point._stop_area.id == stop.id and found:
                    res.append(arrival)
        return res

    def format_departures(self, to=None):
        if to is not None:
            departures = self.departures_going_to(to)
        else:
            departures = self.departures
        res = ""
        for departure in departures:
            res += departure.format() + "\n"
        return res

    def format_arrivals(self, from_=None):
        if from_ is not None:
            arrivals = self.arrivals_coming_from(from_)
        else:
            arrivals = self.arrivals
        res = ""
        for arrival in arrivals:
            res += arrival.format() + "\n"
        return res

    @property
    def departures(self):
        return [
            Stop(**d)
            for d in config.navitia.get(f"stop_areas/{self.id}/departures?data_freshness=realtime").json()["departures"]
        ]

    @property
    def arrivals(self):
        return [
            Stop(**arr)
            for arr in
            config.navitia.get(f"stop_areas/{self.id}/arrivals?data_freshness=realtime").json()["arrivals"]
        ]

    @property
    def schedules(self):
        return [
            StopSchedule(**item)
            for item in
            config.navitia.get(f"stop_areas/{self.id}/stop_schedules?data_freshness=realtime").json()["stop_schedules"]
        ]

    @property
    def routes(self):
        return [
            Route(**item)
            for item in
            config.navitia.get(f"stop_areas/{self.id}/routes").json()["routes"]
        ]

    @property
    def lines(self):
        return [
            Line(**item)
            for item in
            config.navitia.get(f"stop_areas/{self.id}/lines").json()["lines"]
        ]

    @property
    def line_reports(self):
        return [
            item
            for item in
            config.navitia.get(f"stop_areas/{self.id}/line_reports").json()["line_reports"]
        ]


@dataclass
class StopSchedule:
    stop_point:str
    links:str
    date_times:str
    route:str
    additional_informations:str
    display_informations:str

    def __post_init__(self):
        self.route = Route(**self.route)
        self.stop_point = StopPoint(**self.stop_point)
        self.dts = [
            StopScheduleDateTime(**{**dt, "schedule": self})
            for dt in self.date_times
        ]


@dataclass
class Journey:
    status:str
    distances:str
    links:str
    tags:str
    nb_transfers:str
    durations:str
    arrival_date_time:str
    calendars:str
    departure_date_time:str
    requested_date_time:str
    fare:str
    co2_emission:str
    type:str
    duration:str
    sections:str

    def format(self):
        res = ""
        for section in self.sections:
            res += section.format() + "\n"
        return res

    def __post_init__(self):
        def fixup_section(s):
            if 'from' in s:
                s["from_"] = s["from"]
                del s["from"]
            return s
        self.sections = [
            Section(**fixup_section(s)) for s in self.sections
        ]


@dataclass
class Section:
    arrival_date_time:str
    co2_emission:str
    departure_date_time:str
    duration:str
    id:str
    links:str
    type:str
    mode:str = None
    to:str = None
    from_:str = None
    data_freshness:str = None
    additional_informations:str = None
    base_arrival_date_time:str = None
    base_departure_date_time:str = None
    display_informations:str = None
    geojson:str = None
    stop_date_times:str = None

    def format(self):
        return f"""{self.from_.name} ({self.base_departure_date_time}) -> {self.type} -> {self.to.name} ({self.base_arrival_date_time})"""

    def __post_init__(self):
        if self.from_ is not None:
            self.from_ = PTObject(**self.from_)
        if self.to is not None:
            self.to = PTObject(**self.to)


class NavitiaConfig:
    def get(self, path):
        @cache_disk(expire=300)
        def _get(path):
            parameters = "items_per_page=1000"
            if '?' in path:
                path += "&"
            else:
                path += "?"
            path += parameters
            url = f"https://api.navitia.io/v1/coverage/{self.coverage}/{path}"
            LOGGER.info(f"Getting {url}")
            return requests.get(
                url,
                headers={"Authorization": self.key})
        return _get(path)

    def journey(self, from_, to):
        return self.journeys(from_, to)[0]

    def journeys(self, from_, to):
        return [
            Journey(**it)
            for it in self.get(f"journeys?from={from_.id}&to={to.id}&data_freshness=realtime").json()["journeys"]
        ]

    def place(self, q, embedded_types=[]):
        return self.places(q, embedded_types)[0]

    def pt_object(self, q):
        return self.pt_objects(q)[0]

    def places(self, query, embedded_types=[]):
        res = [
            PTObject(**place)
            for place in self.get(f"places?q={query}").json()["places"]
        ]
        if embedded_types:
            res = [
                r for r in res
                if r.embedded_type in embedded_types
            ]
        return res

    def pt_objets(self, query):
        return [
            PTObject(**place)
            for place in self.get(f"pt_objects?q={query}").json()["pt_objects"]
        ]

    def line(self, id):
        content = self.get(f"lines/{id}").json()["lines"][0]
        return Line(**content)


class GetType(ParameterType):
    name = "getparameter"

    @staticmethod
    def list(url='', safe=False, recursive=True):
        urls = defaultdict(set)

        def process_result(res):
            if safe and res is None or not isinstance(res, dict) or 'links' not in res.keys():
                return
            links = res['links']
            api_root = os.path.basename(config.rest.url)
            for v in links.values():
                u, params = re.match(r'.+?{}/([^{{}}]+)({{.*}}|)'.format(api_root), v['href']).groups()
                urls[u].add(params)

        process_result(_get(url, safe, internal=True).json())
        if recursive:
            to_process = set(urls.keys())
            processed = set()
            while to_process:
                for u in to_process:
                    if u != url:
                        process_result(_get(u, True).json())
                processed.update(to_process)
                to_process = set(urls.keys()) - processed
        return list(urls.items())

    def complete(self, ctx, incomplete):
        @cache_disk(expire=3600)
        def _list_rest(*args, **kwargs):
            return self.list(*args, **kwargs)
        url = re.sub('[^/]+$', '', incomplete)
        res = [(u, ' '.join(p)) for u, p in _list_rest(url, True, False) if startswith(u, incomplete)]
        res += [(s[0] + '/', None) for s in res]
        return res



@group()
@param_config(
    "navitia", "--coverage",
    typ=NavitiaConfig,
    default="sncf",
    required=True)
@param_config(
    "navitia", "--key",
    typ=NavitiaConfig,
    default=get_navitia_key,
    required=True)
def navitia():
    pass


class StopParameterType(ParameterType):
    @staticmethod
    def complete(ctx, incomplete):
        if len(incomplete) < 3:
            return []
        try:
            return [
                place.name for place in config.navitia.places(
                    incomplete, embedded_types=["stop_point", "stop_area"])
            ]
        except KeyError:
            return []

    def convert(self, value, param, ctx):
        return config.navitia.place(
            value, embedded_types=["stop_point", "stop_area"])


@navitia.command()
@argument("from_", type=StopParameterType())
@argument("to", type=StopParameterType())
def impact(from_, to):
    print(from_.stop_area.departures_going_to(to)[0].format_impact())


@navitia.command()
@argument("stop", type=StopParameterType())
@option("--to", type=StopParameterType())
def departures(stop, to):
    print(stop.stop_area.format_departures(to=to))


@navitia.command()
@argument("stop", type=StopParameterType())
@option("--from", "from_", type=StopParameterType())
def arrivals(stop, from_):
    print(stop.stop_area.format_arrivals(from_=from_))


@navitia.command()
@argument("journey")
def vehicle_journey_impact(journey):
    print(
        VehicleJourney(**config.navitia.get(f"vehicle_journeys/{journey}").json()["vehicle_journeys"][0]).format_impact()
    )


@navitia.command()
def ipython():
    n = config.navitia
    import IPython
    dict_ = globals()
    dict_.update(locals())
    IPython.start_ipython(argv=[], user_ns=dict_)
