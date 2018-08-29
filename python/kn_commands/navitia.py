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
from fuzzywuzzy import fuzz
from dataclasses import dataclass, field
import requests

from bs4 import BeautifulSoup as soup
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


def extract_vehicle_journey_id(links):
    try:
        return [
            link["id"] for link in links
            if link["type"] == "vehicle_journey"
        ][0]
    except IndexError:
        return None


def get_navitia_key():
    values = get_authenticator(f"navitia_{config.navitia.coverage}_key", required=False, askpass=False)
    if values is not None:
        return values[1]
    else:
        return None


class HasVehicleJourneyMixin:
    @property
    def vehicle_journey_id(self):
        return extract_vehicle_journey_id(self.links)

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
        self.application_periods = [
            {
                "begin": parsetime(application_period["begin"]),
                "end": parsetime(application_period["end"]),
            }
            for application_period in self.application_periods
        ]

    def format_stop(self, stop_id):
        stop = [
            s
            for s in self.impacted_objects[0]["impacted_stops"]
            if s["stop_point"].id == stop_id
        ][0]
        return f"""Impact:
cause: {stop["cause"]}
{stop["base_arrival_time"]} -> {stop["base_departure_time"]}
{stop["amended_arrival_time"]} -> {stop["amended_departure_time"]} ({stop["arrival_status"]}, {stop["stop_time_effect"]}, {stop["departure_status"]})
"""

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

    def __post_init__(self):
        for st in self.stop_times:
            st["arrival_time_today"] = parsetime(datetime.datetime.now().strftime("%Y%m%dT") + st["arrival_time"])
            st["departure_time_today"] = parsetime(datetime.datetime.now().strftime("%Y%m%dT") + st["departure_time"])
            st["stop_point"] = StopPoint(**st["stop_point"])

    @property
    def disruption(self):
        disruptions = [
            d for d in self.disruptions_
            if d.application_periods[0]["begin"] <= self.stop_times[-1]["departure_time_today"]
            and self.stop_times[0]["arrival_time_today"] <= d.application_periods[0]["end"]
        ]
        if disruptions:
            return disruptions[0]
        return None

    def format_impact(self):
        res = self.id + "\n"
        res += self.trip["name"] + "\n"
        disruption = self.disruption
        for st in self.stop_times:
            sp = st["stop_point"]
            trainstation_info = sp.format_trainstation(self.trip["name"])
            res += f"""{sp.name}
 {trainstation_info}
{st["arrival_time_today"].strftime("%m/%d %H:%M")} -> {st["departure_time_today"].strftime("%m/%d %H:%M")}
"""
            if disruption:
                res += disruption.format_stop(sp.id)
            res += """
"""
        return res

    @property
    def stop_points(self):
        return [time["stop_point"] for time in self.stop_times]

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
    fare_zone:str = None
    zip_codes_to_regions: str = field(default_factory=lambda: {
        "01": "auvergne-rhone-alpes",
        "38": "auvergne-rhone-alpes",
        "69": "auvergne-rhone-alpes",
        "42": "auvergne-rhone-alpes",
        "83": "paca",
    })
    address: str = None

    @property
    def tvs(self):
        return self.info_gare["fields"]["tvs"]

    @property
    def info_gare(self):
        @cache_disk(expire=3600*24*365)
        def _get_referentiel_gares_voyageurs():
            url = "https://ressources.data.sncf.com/explore/dataset/referentiel-gares-voyageurs/download?format=json"
            LOGGER.debug(f"Getting {url}")
            r = requests.get(url)
            return json.loads(r.content)

        @cache_disk(expire=3600*24*365)
        def _get_info_gare(name):
            return builtins.max(
                _get_referentiel_gares_voyageurs(),
                key=lambda e: fuzz.ratio(
                    e["fields"]["intitule_plateforme"],
                    name
                )
            )
        return _get_info_gare(self.name)

    def format_trainstation(self, trip_name):
        trainstation_info = self.trainstation_info.get(trip_name, {})
        return f"""quay: {trainstation_info.get("quay", "N/A")}, prob: {trainstation_info.get("problem", "N/A")}"""

    @property
    def trainstation_info(self):
        @cache_disk(expire=60)
        def _get_trainstation_info(tvs):
            url = f"https://www.gares-sncf.com/fr/train-times/{tvs}/departure"
            LOGGER.debug(f"Getting {url}")
            r = requests.get(url)
            trains = json.loads(r.content)["trains"]
            return {
                train["num"]: {
                    **train,
                    "problem": ", ".join([
                        train[key]
                        for key in ["etat", "retard", "infos"]
                        if train[key] != ""
                    ]) or None,
                    "hour": train["heure"],
                    "destination": train["origdest"],
                    "number": train["num"],
                    "mode": train["type"],
                    "quay": train["voie"],
                }
                for train in trains
            }
        return _get_trainstation_info(self.tvs)

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
            config.navitia.get(f"stop_areas/{self.id}/stop_schedules?data_freshness=realtime").json()["stop_schedules"]
        ]


@dataclass
class Stop(HasVehicleJourneyMixin):
    display_informations: str
    stop_point: dict
    route: dict
    links: list
    stop_date_time: dict

    @property
    def way_schedules(self):
        return [
            {
                "stop_point": row.stop_point,
                "dt": date_time,
            }
            for schedule in self.route.schedules
            for row in schedule.rows
            for date_time in row.date_times
            if extract_vehicle_journey_id(date_time["links"]) == self.vehicle_journey_id
        ]

    @property
    def format_way_schedules(self):
        res = ""
        disruption = self.disruption
        for ws in self.way_schedules:
            trainstation_info = ws['stop_point'].format_trainstation(self.vehicle_journey.trip["name"])
            res += f"""{ws["stop_point"].name}
  {trainstation_info}
{parsetime(ws["dt"]["base_date_time"]).strftime("%m/%d %H:%M")} ({parsetime(ws["dt"]["date_time"]).strftime("%m/%d %H:%M")})
"""

            if disruption is not None:
                res += disruption.format_stop(ws["stop_point"].id)
            res += """
"""
        return res

    @property
    def disruption(self):
        disruptions = [
            d for d in self.vehicle_journey.disruptions_
            if d.application_periods[0]["begin"] <= self.stop_date_time["departure_date_time"]
            and self.stop_date_time["arrival_date_time"] <= d.application_periods[0]["end"]
        ]
        if disruptions:
            disruption = disruptions[0]
            return [
                impact for impact in disruption.impacted_stops
                if impact["stop_point"].id == self.stop_point.id
            ][0]
        return None

    @property
    def trainstation_info(self):
        headsign = int(self.display_informations["headsign"])
        # the headsign from display_informations may differ from the one given
        # in the trainstation_info, due to
        # http://maligne-ter.com/st-etienne-lyon/la-numerotation-des-trains-pair-ou-impair/
        # . Either navitia, or sncf did it wrong, but I can't tell yet
        odd = lambda x: x % 2 == 1
        even = lambda x: x % 2 == 0
        even_headsign = str(headsign if even(headsign) else headsign - 1)
        odd_headsign = str(headsign if odd(headsign) else headsign + 1)
        ti = self.stop_point.trainstation_info
        return ti.get(even_headsign) or ti.get(odd_headsign) or {}

    @property
    def quay(self):
        return self.trainstation_info.get("quay", "N/A")

    @property
    def problem(self):
        return self.trainstation_info.get("problem", "N/A")

    def format(self):
        res = f"""{self.display_informations.direction}
quay: {self.quay}, prob: {self.problem}, vehicle: {self.vehicle_journey.id}
{self.stop_date_time['base_arrival_date_time'].strftime("%m/%d %H:%M")} -> {self.stop_date_time['base_departure_date_time'].strftime("%m/%d %H:%M")}
{self.stop_date_time['arrival_date_time'].strftime("%m/%d %H:%M")} -> {self.stop_date_time['departure_date_time'].strftime("%m/%d %H:%M")}
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
            LOGGER.debug(f"Getting {url}")
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

    def vehicle_journey(self, journey):
        return VehicleJourney(**self.get(f"vehicle_journeys/{journey}").json()["vehicle_journeys"][0])

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
@argument("stop", type=StopParameterType())
def way_impact(journey, stop):
    d = [
        dep
        for dep in stop.stop_area.departures
        if dep.vehicle_journey.id == journey
    ][0]
    print(d.format_way_schedules)


@navitia.command()
@argument("journey")
def vehicle_journey_impact(journey):
    print(
        config.navitia.vehicle_journey(journey).format_impact()
    )


@navitia.command()
def ipython():
    n = config.navitia
    import IPython
    dict_ = globals()
    dict_.update(locals())
    IPython.start_ipython(argv=[], user_ns=dict_)
