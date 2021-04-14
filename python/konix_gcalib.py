#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import cmd
import urllib.request
import markdown
import json
import os
import re
import sys
import pprint
import html2text
import redis
import functools
import shlex
import datetime
import time
import collections
import pytz
import parsedatetime
import dateutil.parser
import dateutil
import requests
import konix_collections
import urllib.parse
import hashlib
from dateutil import parser

from konix_time_helper import get_local_timezone, date_to_local

import logging
logging.basicConfig()
LOGGER = logging.getLogger(__file__)
LOGGER.setLevel(logging.DEBUG)
pp = pprint.PrettyPrinter(indent=4, width=200)

import readline
# handle the history
histfile = os.environ.get("KONIX_GCAL_HISTORY",
                          os.path.expanduser("~/.konix_gcal_history"))
try:
    readline.read_history_file(histfile)
except IOError:
    pass
import atexit
atexit.register(readline.write_history_file, histfile)
del histfile

readline.set_completer_delims(
    readline.get_completer_delims().replace("/", "").replace("-", "")
)

DISCOVERY_URI = 'https://www.googleapis.com/discovery/v1/apis/{api}/{apiVersion}/rest'.format(
                     api="calendar",
                     apiVersion="v3")

EVENT_STRFTIME="%Y-%m-%dT%H:%M:%S.000%z"
EVENT_URL_STRFTIME="%Y-%m-%dT%H:%M:%SZ"
EVENT_STRFTIME_ALL_DAY="%Y-%m-%d"


class OutOfDateError(Exception):
    pass


def lazy(expire_key):
    def real_decorator(func):
        @functools.wraps(func)
        def wrapper(self, *args, **kwargs):
            if self.db.get(self.db_name_transform(expire_key)):
                LOGGER.debug("{} up to date".format(self.db_name_transform(expire_key)))
            else:
                LOGGER.debug("{} out of date (reeval)".format(self.db_name_transform(expire_key)))
                return func(self, *args, **kwargs)
        return wrapper
    return real_decorator

PROVIDERS_KEYS = {}


class EventNotFound(BaseException):
    pass


def needs(expire_key):
    def real_decorator(func):
        @functools.wraps(func)
        def wrapper(self, *args, **kwargs):
            if not (
                    self.db.get(self.db_name_transform(expire_key))
                    and expire_key in PROVIDERS_KEYS
            ):
                LOGGER.debug("Calling the provider of the needed key {}".format(self.db_name_transform(expire_key)))
                provider, interactive = PROVIDERS_KEYS[expire_key]
                if interactive:
                    LOGGER.error("You must call {}".format(provider))
                    raise Exception("You must call {}".format(provider))
                provider(self)
            if self.db.get(self.db_name_transform(expire_key)):
                return func(self, *args, **kwargs)
            else:
                LOGGER.error("{} is needed".format())
        return wrapper
    return real_decorator

def provides(key, interactive=False):
    def real_decorator(func):
        @functools.wraps(func)
        def wrapper(self, *args, **kwargs):
            return func(self, *args, **kwargs)
        global PROVIDERS_KEYS
        PROVIDERS_KEYS[key] = (wrapper, interactive,)
        return wrapper
    return real_decorator

PROMPT="GCal({calendar_id}, {event_list_extra_query}, {time_min}, {time_max})\n> "

class GCall(cmd.Cmd, object):
    def __init__(self, make_place=False, account=""):
        cmd.Cmd.__init__(self)
        self.account = account
        self.db = redis.StrictRedis(decode_responses=True)
        self.client_id = self.db.get("client_id")
        self.client_secret = self.db.get("client_secret")
        assert self.client_id, "redis-cli set client_id <yourid> (https://console.developers.google.com/project/<yourapp>/apiui/credential). Use a native application type."
        assert self.client_secret, "redis-cli set client_secret <yoursecret> (https://console.developers.google.com/project/<yourapp>/apiui/credential). Use a native application type."
        self.calendar_filter = "'{search_term}'.lower() in x.summary.lower()"
        self.event_filter = "'{search_term}'.lower() in x.summary.lower()"
        self.calendar_formatter = "str([x.id, x.summary])"
        self.event_formatter = r'"{}-{} ({}), {}, {}".format(x.startdate.strftime("%a %d/%m %H:%M"), x.enddate.strftime("%H:%M"), str(x.duration), x.summary, x.id,)'
        self.get_api()
        self.setup_types()
        self.updatable_data = [
            "summary",
            "start",
            "end",
            "status",
            "attendees",
            "description"
        ]
        self._make_place = make_place
        self.set_prompt()

    def parse_time(self, value):
        if isinstance(value, datetime.datetime):
            return value, None
        calendar_id = self.db.get(self.calendar_id_name)
        calendar = self.get_calendar(calendar_id)
        tz = pytz.timezone(calendar.timeZone)
        cal = parsedatetime.Calendar()
        return cal.parseDT(value,
                           sourceTime=datetime.datetime.today(),
                           tzinfo=tz)

    @property
    def time_min_name(self):
        return self.db_name_transform("time_min")

    @property
    def time_min(self):
        result = self.db.get(self.time_min_name)
        if result:
            result = dateutil.parser.parse(result)
        return result

    @time_min.setter
    def time_min(self, value):
        if value == "" or value is None or value == "never":
            self.db.delete(self.time_min_name)
        else:
            date_, _ = self.parse_time(value)
            self.db.set(self.time_min_name, date_.isoformat())
        self.do_clear_list_event()
        self.set_prompt()

    def do_time_min(self, value):
        self.time_min = value
        print("Set TimeMin to {}".format(self.time_min))

    @property
    def time_max_name(self):
        return self.db_name_transform("time_max")

    @property
    def time_max(self):
        result = self.db.get(self.time_max_name)
        if result:
            result = dateutil.parser.parse(result)
        return result

    @time_max.setter
    def time_max(self, value):
        if value == "" or value is None or value == "never":
            self.db.delete(self.time_max_name)
        else:
            date_, _ = self.parse_time(value)
            self.db.set(self.time_max_name, date_.isoformat())
        self.do_clear_list_event()
        self.set_prompt()

    def do_time_max(self, value):
        self.time_max = value
        print("Set Timemax to {}".format(self.time_max))

    @property
    @needs("calendars")
    def calendars(self):
        calendars_string = self.db.get(self.calendars_name)
        if calendars_string:
            data = json.loads(calendars_string)
            calendars = [
                CalendarListEntry(**item)
                for item in data["items"]
            ]
        else:
            calendars = []
        return calendars


    def format(self, object_):
        object_formatters = {
            "Event": self.event_formatter,
            "CalendarListEntry": self.calendar_formatter
        }
        formatter = object_formatters[object_.__class__.__name__]
        formatter = eval("lambda x:" + formatter)
        return formatter(object_)

    def pprint(self, object_):
        pp.pprint(self.format(object_))

    def setup_types(self):
        global Calendar, CalendarListEntry, Event
        self.types = {
            "CalendarListEntry" : {
                "keys": self.api["schemas"]["CalendarListEntry"]["properties"].keys()
            },
            "Event" : {
                "keys": self.api["schemas"]["Event"]["properties"].keys()
            }
        }
        CalendarListEntry = konix_collections.namedtuples_default_values(
            "CalendarListEntry",
            self.types["CalendarListEntry"]["keys"],
            {k: "" for k in self.types["CalendarListEntry"]["keys"]}
        )

        Event = konix_collections.namedtuples_default_values(
            "Event",
            sorted(list(self.types["Event"]["keys"]) + ["calendar_id"]),
            {k: "" for k in self.types["Event"]["keys"]}
        )

        def print_diary(self):
            print("{}-{} {} ({}) [{}]".
                  format(
                      self.startdate.strftime("%Y-%m-%d %H:%M"),
                      self.enddate.strftime("%H:%M"),
                      self.summary,
                      self.htmlLink,
                      self.id
                  )
            )

        @property
        def org_mode_timestamp(self):
            stamp = "<{}-{}".format(
                self.startdate.strftime("%Y-%m-%d %H:%M"),
                self.enddate.strftime("%H:%M"),
            )
            if self.recurrence:
                weekly = re.compile("RRULE:FREQ=WEEKLY(;COUNT=[0-9]+)?(;INTERVAL=(?P<interval>[0-9]+))?(;UNTIL=[^;]+)?(;WKST=[^;]+)?;BYDAY=(.+)")
                assert len(self.recurrence) == 1, "Cannot handle several recurrences yet"
                rule = self.recurrence[0]
                if weekly.match(rule):
                    stamp += " ++{}w".format(
                        weekly.match(rule).group("interval") or 1
                    )
                else:
                    raise NotImplementedError(str(self.recurrence))
            stamp += ">"
            return stamp

        @property
        def my_response_status(self):
            me_as_attendee = [
                attendee
                for attendee in self.attendees
                if attendee.get("email") == self.calendar_id
            ]
            if me_as_attendee:
                assert len(me_as_attendee) == 1
                return me_as_attendee[0]["responseStatus"]
            else:
                return ""

        @property
        def text_description(self):
            description = self.description.strip()
            try:
                description = html2text.html2text(description)
            except:
                description = "(Not valid html) " + description
            return description

        @property
        def tags(self):
            res = ["gcalgenerated"]
            if self.colorId == "3":
                res.append("discret")
            if self.my_response_status:
                res.append(self.my_response_status)
            if self.recurringEventId:
                res.append("recurrent")
            return res

        @property
        def org_mode(self):
            attendees_self = [a for a in self.attendees if a.get("self")]
            if attendees_self:
                optional = attendees_self[0].get("optional", False)
            else:
                optional = False
            return """* [[{}][{}]]{}{}
:PROPERTIES:
:ID: {}
:CALENDAR_ID: {}
:ACCOUNT_NAME: {}
:LOCATION: {}
:ORGANIZER: {}
:CREATION_DATE: {}
:UPDATED: {}
:UPDATEDRAW: {}
:OPTIONAL: {}
:END:
{}
By: {}
Attendees:
{}

{}
** Raw   :noexport:structure:
#+BEGIN_EXAMPLE
{}
#+END_EXAMPLE
""".format(
    self.htmlLink,
    self.summary.replace("[", "{").replace("]", "}") or "NoSummary",
    (f" ([[{self.hangoutLink}][Hangout link]])" if self.hangoutLink else ""),
    ("    :{}:".format(":".join(self.tags)) if self.tags else ""),
    self.id,
    self.calendar_id,
    self.account,
    self.location,
    self.organizer.get("displayName", self.organizer.get("email", "NA")),
    date_to_local(parser.parse(self.created)).strftime("[%Y-%m-%d %H:%M]"),
    date_to_local(parser.parse(self.updated)).strftime("[%Y-%m-%d %H:%M]"),
    self.updated,
    "t" if optional else "nil",
    self.org_mode_timestamp,
    self.organizer.get("displayName", self.organizer.get("email", "NA")),
    "\n".join(
        map(
            lambda a: " - {} ({}) ({})".format(
                a.get("email", "NA"),
                a.get("responseStatus"),
                a.get("comment", "no comment"),
            ),
            self.attendees)),
    re.sub("^\*", ",*", self.text_description.strip(), flags=re.MULTILINE),
    re.sub("^\*", ",*", str(self).strip(), flags=re.MULTILINE),
)

        @property
        def startdate(self):
            if self.start.get("dateTime"):
                date = dateutil.parser.parse(self.start.get("dateTime")
                )
            else:
                date = dateutil.parser.parse(
                    self.start.get("date")
                )
            if date.tzinfo is None:
                date = date.replace(tzinfo=get_local_timezone())
            return date

        @property
        def enddate(self):
            if self.end.get("dateTime"):
                date = dateutil.parser.parse(self.end.get("dateTime")
                )
            else:
                date = dateutil.parser.parse(
                    self.end.get("date")
                )
            if date.tzinfo is None:
                date = date.replace(tzinfo=get_local_timezone())
            return date

        @property
        def uid(self):
            """Id that is uniq between recurring events"""
            return re.match(
                "^(?P<id>.+?)(_.{16})?$", self.id).group("id")

        @property
        def duration(self):
            return self.enddate-self.startdate

        def event_hash(self):
            return int(hashlib.sha1(self.id.encode("utf-8")).hexdigest(), 16)

        Event.startdate = startdate
        Event.enddate = enddate
        Event.duration = duration
        Event.uid = uid
        Event.account = self.account
        Event.print_diary = print_diary
        Event.org_mode_timestamp = org_mode_timestamp
        Event.org_mode = org_mode
        Event.tags = tags
        Event.__hash__ = event_hash
        Event.my_response_status = my_response_status
        Event.text_description = text_description

        self.types["CalendarListEntry"]["class"] = CalendarListEntry
        self.types["Event"]["class"] = Event
        self.Event = Event

    @property
    @needs("api")
    def api(self):
        return json.loads(self.db.get("api"))

    @provides("api")
    def get_api(self):
        res = requests.get(DISCOVERY_URI)
        assert res.status_code == 200
        self.db.set("api", res.text)

    def get_attr(self, key):
        return eval("self.{}".format(key))

    def set_prompt(self):
        dict_ = self.__dict__.copy()
        added = {
            key: self.get_attr(key)
            for key in [
                    "calendar_id",
                    "event_list_extra_query",
                    "time_min",
                    "time_max",
            ]
        }
        dict_.update(added)
        self.prompt = PROMPT.format(
            **added
        )

    @property
    def event_list_extra_query(self):
        return self.db.get("event_list_extra_query")

    def do_init(self, line=None):
        self.do_get_user_permission()
        self.do_get_access_token()

    def do_show_event(self, event_id):
        print(self.get_event(event_id))

    def do_dump_org(self, event_id):
        print(self.get_event(event_id).org_mode)

    def do_clear_token(self, line=None):
        self.db.delete(self.access_token_name)
        self.db.delete(self.refresh_token_name)

    def db_name_transform(self, name):
        if name in ["access_token",
                    "refresh_token",
                    "user_code",
                    "device_code",
                    "all_events",
                    "calendar_id",
                    "calendars",
                    "time_min",
                    "time_max",
                ]:
            return "{}_{}".format(self.account, name)
        else:
            return name

    @property
    def access_token_name(self):
        return self.db_name_transform("access_token")

    @property
    def user_code_name(self):
        return self.db_name_transform("user_code")

    @property
    def device_code_name(self):
        return self.db_name_transform("device_code")

    @property
    def refresh_token_name(self):
        return self.db_name_transform("refresh_token")

    @property
    def all_events_name(self):
        return self.db_name_transform("all_events")

    @property
    def calendar_id_name(self):
        return self.db_name_transform("calendar_id")

    @property
    def calendar_id(self):
        return self.db.get(self.calendar_id_name)

    @property
    def calendars_name(self):
        return self.db_name_transform("calendars")

    def do_clear_list_event(self, line=None):
        self.db.delete(self.all_events_name)

    #@lazy("access_token")
    @provides("access_token")
    def refresh_token(self):
        LOGGER.debug("Refreshing access token")
        req = urllib.request.Request(
            url='https://www.googleapis.com/oauth2/v4/token',
            data='client_id={}&client_secret={}&refresh_token={}&grant_type=refresh_token'.format(
                self.client_id,
                self.client_secret,
                self.db.get(self.refresh_token_name)
            ).encode("utf-8"))
        f = urllib.request.urlopen(req)
        assert f.code == 200
        data = json.loads(f.read().decode("utf-8"))
        self.db.set(self.access_token_name, data["access_token"])
        self.db.expire(self.access_token_name, 5) # int(data["expires_in"]))
        self.db.set("token_type", data["token_type"])
        assert self.db.get("token_type") == "Bearer"

    def do_refresh_token(self, line=""):
        self.refresh_token()

    @provides("calendars")
    @needs("access_token")
    def all_calendars(self):
        req = urllib.request.Request(
            url='https://www.googleapis.com/calendar/v3/users/me/calendarList',
            headers={"Authorization": "{} {}".format(
                self.db.get("token_type"),
                self.db.get(self.access_token_name))}
        )
        f = urllib.request.urlopen(req)
        assert f.code == 200
        calendars_string = f.read().decode("utf-8")
        self.db.set(self.calendars_name, calendars_string)
        data = json.loads(calendars_string)
        calendars = [
            CalendarListEntry(**item)
            for item in data["items"]
        ]
        return calendars

    @needs("access_token")
    def list_calendars(self, search_term=None):
        if search_term:
            filter_ = eval("lambda x:" + self.calendar_filter.format(search_term=search_term))
        calendars = [
            cal
            for cal in self.all_calendars()
            if not search_term or filter_(cal)
        ]
        return calendars

    @needs("access_token")
    def list_calendars_pandas(self, search_term=None):
        import pandas
        return pandas.DataFrame(
            [ce._asdict().values() for ce in self.list_calendars(search_term)],
            columns=self.api["schemas"]["CalendarListEntry"]["properties"].keys()
        )

    @needs("access_token")
    def do_list_calendars(self, search_term=None):
        calendars = self.list_calendars(search_term)
        formatter = eval("lambda x:" + self.calendar_formatter)
        pp.pprint([formatter(calendar) for calendar in calendars])

    def do_show_calendar(self, calendar_id):
        if not self.calendars:
            print("Run list_calendars first")
            return
        pp.pprint(*[calendar for calendar in self.calendars if calendar.id == calendar_id])

    def complete_show_calendar(self, text, line, begidx, endidx):
        return [
            calendar.id for calendar in self.calendars
            if calendar.id.startswith(text)
        ]

    def help_show_calendar(self):
        print("{}".format(calendar_keys))

    def do_map_show_calendar(self, keys):
        if not self.calendars:
            print("Run list_calendars first")
            return
        keys = shlex.split(keys)
        pp.pprint(
            [
                {
                    key:calendar.__getattribute__(key)
                    for key in keys
                }
                for calendar in self.calendars
            ]
        )

    def help_map_show_calendar(self):
        print("{}".format(calendar_keys))

    def get_calendar(self, id_):
        return [calendar
                for calendar in self.calendars
                if calendar.id == id_
        ][0]

    def do_calendar_filter(self, line):
        if line:
            self.calendar_filter = line
        else:
            print(self.calendar_filter)

    def do_event_filter(self, line):
        if line:
            self.event_filter = line
        else:
            print(self.event_filter)

    def do_event_list_extra_query(self, line):
        """Try: &timeMin=2014-11-30T00:00:00Z&timeMax=2014-12-30T00:00:00Z"""
        if line:
            self.db.set("event_list_extra_query", line)
            print("Clearing the list of events")
            self.do_clear_list_event()
        else:
            print(self.event_list_extra_query)
        self.set_prompt()

    def do_del_event_list_extra_query(self, line):
        self.db.delete("event_list_extra_query")
        print("Clearing the list of events")
        self.do_clear_list_event()
        self.set_prompt()

    def do_calendar_formatter(self, line):
        if line:
            self.calendar_formatter = line
        else:
            print(self.calendar_formatter)

    def do_event_formatter(self, line):
        if line:
            self.event_formatter = line
        else:
            print(self.event_formatter)

    def do_find_calendar(self, line):
        if not self.calendars:
            print("Run list_calendars first")
            return
        fil = eval("lambda x:" + line)
        pp.pprint(
            [
                calendar
                for calendar in self.calendars
                if fil(calendar)
            ]
        )

    def do_ipython(self, line=None):
        import IPython
        IPython.start_ipython(argv=[], user_ns=locals())

    def help_find_calendar(self):
        print("{}".format(calendar_keys))

    @provides("calendar_id", True)
    def do_select_calendar(self, calendar_id):
        calendar_id = shlex.split(calendar_id)[0]
        if calendar_id:
            self.db.set(self.calendar_id_name, calendar_id)
            self.db.delete(self.all_events_name)
        else:
            self.db.delete(self.calendar_id_name)
        # sanity check: attempt to get the calendar
        self.get_calendar(calendar_id)
        self.set_prompt()

    def complete_select_calendar(self, text, line, begidx, endidx):
        return [
            calendar.id for calendar in self.calendars
            if calendar.id.startswith(text)
        ]

    @needs("calendar_id")
    def do_describe_calendar(self, line):
        calendar_id = self.db.get(self.calendar_id_name)
        formatter = eval("lambda x:" + self.calendar_formatter)
        print(formatter([cal
               for cal in self.list_calendars()
               if cal.id == calendar_id
           ][0]))

    @needs("calendar_id")
    @needs("access_token")
    def get_event(self, event_id):
        calendar_id = self.db.get(self.calendar_id_name)
        req = urllib.request.Request(
            url='https://www.googleapis.com/calendar/v3/calendars/{}/events/{}'.format(
                calendar_id,
                event_id
            ),
            headers={"Content-Type": "application/json",
                     "Authorization": "{} {}".format(
                self.db.get("token_type"),
                self.db.get(self.access_token_name))}
        )
        try:
            f = urllib.request.urlopen(req)
        except urllib.error.HTTPError:
            raise EventNotFound()
        assert f.code == 200
        data = json.loads(f.read().decode("utf-8"))
        data["calendar_id"] = self.calendar_id
        return Event(**data)

    @needs("access_token")
    def del_event(self, event_id):
        calendar_id = self.db.get(self.calendar_id_name)
        req = urllib.request.Request(
            url='https://www.googleapis.com/calendar/v3/calendars/{}/events/{}'.format(
                calendar_id,
                event_id
            ),
            headers={"Content-Type": "application/json",
                     "Authorization": "{} {}".format(
                         self.db.get("token_type"),
                         self.db.get(self.access_token_name))},
            method="DELETE"
        )
        f = urllib.request.urlopen(req)
        assert f.code == 204
        self.db.delete(self.all_events_name)

    def do_del_event(self, event_id):
        self.del_event(event_id)

    @needs("access_token")
    def get_events(self, calendar_id, extra_query=None):
        def get_events(page_token=None):
            url='https://www.googleapis.com/calendar/v3/calendars/{}/events?maxResults=2500&singleEvents=True'.format(
                urllib.parse.quote(calendar_id)
            )
            if page_token:
                url += "&pageToken={}".format(page_token)
            if extra_query:
                url += extra_query
            if self.time_min:
                url += "&timeMin={}T00:00:00Z".format(
                    self.time_min.strftime("%Y-%m-%d")
                )
            if self.time_max:
                url += "&timeMax={}T00:00:00Z".format(
                    self.time_max.strftime("%Y-%m-%d")
                )
            req = urllib.request.Request(
                url=url,
                headers={"Content-Type": "application/json",
                         "Authorization": "{} {}".format(
                             self.db.get("token_type"),
                             self.db.get(self.access_token_name))}
            )
            f = urllib.request.urlopen(req)
            assert f.code == 200
            data = json.loads(f.read().decode("utf-8"))
            return data
        data = get_events()
        items = data["items"]
        while data.get("nextPageToken", None):
            data = get_events(page_token=data.get("nextPageToken", None))
            items += data["items"]
        for i in items:
            i["calendar_id"] = self.calendar_id
        return items

    @provides("all_events")
    def list_all_events(self):
        calendar_id = self.db.get(self.calendar_id_name)
        if not calendar_id:
            return None
        events = self.get_events(calendar_id, self.event_list_extra_query)
        self.db.set(self.all_events_name, json.dumps(events))

    @needs("calendar_id")
    @needs("all_events")
    def list_events(self, search_terms=""):
        items = json.loads(self.db.get(self.all_events_name))
        events = sorted([
            Event(**i)
            for i in items
            ], key=lambda e: e.startdate)
        search_terms = shlex.split(search_terms)

        for search_term in search_terms:
            filter_ = eval("lambda x: " + self.event_filter.format(search_term=search_term))
            events = [e for e in events if filter_(e)]

        return events

    def list_events_pandas(self, search_terms=""):
        res = pandas.DataFrame(
            [
                list(event._asdict().values()) + [
                    event.startdate,
                    event.enddate,
                    event.duration,
                ]
                for event in self.list_events(search_terms)
            ],
            columns=list(self.api["schemas"]["Event"]["properties"].keys()) + [
                "calendarid",
                "startdate",
                "enddate",
                "duration",
            ]
        )
        res.startdate = pandas.to_datetime(res.startdate, utc=True)
        res.enddate = pandas.to_datetime(res.enddate, utc=True)
        res.duration = res.duration
        return res

    def setup_blaze(self):
        import blaze
        from datashape import discover, from_numpy
        columns = list(self.api["schemas"]["Event"]["properties"].keys()) + [
            "calendar_id",
            "startdate",
            "enddate",
            "duration",
        ]
        calendar_id = self.db.get(self.calendar_id_name)
        @discover.register(self.Event)
        def discover_events(event, **kwargs):
            df = pandas.DataFrame(
                [
                    list(event._asdict().values()) + [
                        event.startdate,
                        event.enddate,
                        event.duration,
                    ]
                    for event in [event,]
                ],
                columns=columns,
            )
            shape = (len(df),)
            dtype = df.values.dtype
            return from_numpy(shape, dtype)

        from odo import convert
        @convert.register(pandas.DataFrame, self.Event)
        def event_to_dataframe(event, **kwargs):
            df = pandas.DataFrame(
                [
                    list(event._asdict().values()) + [
                        event.startdate,
                        event.enddate,
                        event.duration,
                    ]
                    for event in [event,]
                ],
                columns=columns,
            )
            return df

    @needs("access_token")
    def do_list_events(self, search_term=None):
        if not self.db.get(self.calendar_id_name):
            print("Use the command select_calendar first")
            return
        events = self.list_events(search_term)
        formatter = eval("lambda x:" + self.event_formatter)
        pp.pprint([formatter(event) for event in events])

    @needs("access_token")
    def add_event(self,
                  title,
                  where,
                  when,
                  duration,
                  description="",
                  attendees_emails=None,
                  make_place=None,
                  sendNotifications=False,
                  reminders=None,
              ):
        if make_place is None:
            make_place = self._make_place
        calendar_id = self.db.get(self.calendar_id_name)

        attendees_emails = attendees_emails or []
        sendNotifications = "true" if sendNotifications else "false"
        if not calendar_id:
            print("Use the command select_calendar first")
            return
        start, flag = self.parse_time(when)
        time_dict = re.match('^\s*((?P<hours>\d+)\s*h)?\s*((?P<minutes>\d+)\s*m?)?\s*((?P<seconds>\d+)\s*s)?\s*$',
                             duration).groupdict()
        duration = datetime.timedelta(seconds=int(time_dict['seconds'] or "0"),
                             minutes=int(time_dict['minutes'] or "0"),
                             hours=int(time_dict['hours'] or "0"))
        end = start + duration
        if make_place:
            self.make_place(calendar_id, start, end)
        if flag == 0:
            start = {
                "date": start.strftime(EVENT_STRFTIME_ALL_DAY),
            }
            end = {
                "date": end.strftime(EVENT_STRFTIME_ALL_DAY),
            }
        else:
            start = {
                "dateTime": start.strftime(EVENT_STRFTIME),
            }
            end = {
                "dateTime": end.strftime(EVENT_STRFTIME),
            }
        attendees = [{"email": attendee_email} for attendee_email in attendees_emails]
        event = {
            "summary" : title,
            "location" : where,
            "description": markdown.markdown(description),
            "start" : start,
            "end" : end,
            "attendees": attendees,
        }
        if not reminders is None:
            event["reminders"] = {
                "useDefault": False,
                "overrides": [
                    {
                        "method": "email",
                        "minutes": reminder
                    }
                    for reminder in reminders
                ]
            }

        req = urllib.request.Request(
            url='https://www.googleapis.com/calendar/v3/calendars/{}/events?sendNotifications={}'.format(
                calendar_id,
                sendNotifications,
            ),
            data=json.dumps(event).encode("utf-8"),
            headers={"Content-Type": "application/json",
                     "Authorization": "{} {}".format(
                self.db.get("token_type"),
                self.db.get(self.access_token_name))}
        )
        f = urllib.request.urlopen(req)
        assert f.code == 200
        data = json.loads(f.read().decode("utf-8"))
        event = Event(**data)
        return event

    def make_place(self, calendar_id, start, end):
        # get the events in a reasonable range around start and end
        extra_query = "&timeMin={}T00:00:00Z&timeMax={}T23:59:59Z".format(
            start.strftime("%Y-%m-%d"),
            end.strftime("%Y-%m-%d"),
        )
        items = json.loads(self.get_events(calendar_id, extra_query))
        events = [
            Event(**i)
            for i in items
            ]

        # an event to move start before the end and end after the start
        events = [event for event in events
                  if event.startdate <= end
                  and event.enddate >= start
                  ]
        for event in events:
            # there are 4 cases
            # the event is around start, end. I need to split it in two
            if event.startdate < start and event.enddate > end:
                new_start = end
                new_end = event.enddate
                new_duration = new_end - new_start
                self.add_event(event.summary, event.location,
                               new_start.strftime("%m/%d/%YT%H:%M:%S"),
                               "{}s".format(int(new_duration.total_seconds())),
                               event.description,
                               make_place=False)
                new_values = { "end" :
                             {
                                 'dateTime' : start.strftime(EVENT_STRFTIME)
                             }
                }
                self._update_event(calendar_id, event, new_values)
            # the event is inside start, end, it must be removed
            elif event.startdate >= start and event.enddate <= end:
                self.del_event(event.id)
            # the event overlap not entirely with the start, end
            # if the event is after
            elif event.startdate >= start:
                new_values = { "start" :
                               {
                                   'dateTime' : end.strftime(EVENT_STRFTIME)
                               }
                           }
                self._update_event(calendar_id, event, new_values)
            # if the event is before
            elif event.enddate <= end:
                new_values = { "end" :
                               {
                                   'dateTime' : start.strftime(EVENT_STRFTIME)
                               }
                           }
                self._update_event(calendar_id, event, new_values)

    def accept(self, event, comment="", updated=None):
        data = {
            "attendees": (event.attendees or []) + [
                {
                    "email": self.calendar_id,
                    "responseStatus": "accepted",
                    "comment": comment,
                }
            ]
        }
        return self._update_insist(
            event, data, send_notif=True if comment else False,
            updated=updated
        )

    def decline(self, event, why="", updated=None):
        self._update_insist(
            event,
            {
                "attendees": (event.attendees or []) + [
                    {
                        "email": self.calendar_id,
                        "responseStatus": "declined",
                        "comment": why,
                    }
                ]
            },
            send_notif=True if why else False,
            updated=updated,
        )

    def tentative(self, event, comment="", updated=None):
        self._update_insist(
            event,
            {
                "attendees": (event.attendees or []) + [
                    {
                        "email": self.calendar_id,
                        "responseStatus": "tentative",
                        "comment": comment,
                    }
                ]
            },
            send_notif=True if comment else False,
            updated=updated,
        )

    def do_accept(self, id, comment="", updated=""):
        self.accept(
            self.get_event(id),
            comment,
            None if not updated else updated,
        )

    def do_decline(self, id, why="", updated=""):
        self.decline(
            self.get_event(id),
            why,
            None if not updated else updated,
        )

    def do_tentative(self, id, comment="", updated=""):
        self.tentative(
            self.get_event(id),
            comment,
            None if not updated else updated,
        )

    def _update_insist(self, event, data, send_notif=False, updated=None):
        self.refresh_token()
        try:
            self._update_event(
                self.calendar_id,
                event,
                data,
                send_notif=send_notif,
                updated=updated,
            )
        except urllib.error.HTTPError:
            self._update_event(
                event.organizer["email"],
                event,
                data,
                send_notif=send_notif,
                updated=updated,
            )

    def _update_event(self, calendar_id, event, new_values,
                      send_notif=False, updated=None):
        if updated is not None and event.updated != updated:
            raise OutOfDateError()
        dict_ = dict(event._asdict())
        dict_ = {
            k: dict_[k]
            for k in dict_
            if k in self.updatable_data
        }
        dict_.update(new_values)
        url = 'https://www.googleapis.com/calendar/v3/calendars/{}/events/{}'.format(
            calendar_id,
            event.id
        )
        if send_notif:
            url += "?sendNotifications=true"
        req = urllib.request.Request(
            url=url,
            data=json.dumps(
                dict_
            ).encode("utf-8"),
            headers={"Content-Type": "application/json",
                     "Authorization": "{} {}".format(
                         self.db.get("token_type"),
                         self.db.get(self.access_token_name))},
            method='PATCH'
        )
        f = urllib.request.urlopen(req)
        assert f.code == 200
        data = json.loads(f.read().decode("utf-8"))
        event = Event(**data)
        return event

    @needs("access_token")
    @needs("calendar_id")
    def update_event(self, event_id, new_summary):
        calendar_id = self.db.get(self.calendar_id_name)
        event = self.get_event(event_id)
        new_values = {"summary" : new_summary}
        return self._update_event(calendar_id, event, new_values)

    def do_update_event(self, line):
        event_id, new_summary = shlex.split(line)
        event = self.update_event(event_id, new_summary)
        print(event)

    def sed_events(self, search_terms, regexp, replace):
        events = self.list_events(search_terms)
        new_events = []
        for event in events:
            new_summary = re.sub(regexp, replace, event.summary)
            if new_summary != event.summary:
                LOGGER.info("Replacing summary of event {} from '{}' to '{}'".format(
                    event.id,
                    event.summary,
                    new_summary
                ))
                new_events.append(self.update_event(event.id, new_summary))
        return new_events

    def do_sed_events(self, line):
        search_terms, regexp, replace = shlex.split(line)
        events = self.sed_events(search_terms, regexp, replace)
        pp.pprint([self.format(event) for event in events])

    @needs("access_token")
    def do_add_event(self, line):
        """title, where, when, duration, description (optional)"""
        (title, where, when, duration, *description) = shlex.split(line)
        description = description[0] if description else ""
        event = self.add_event(title, where, when, duration, description)
        print(event)

    def do_EOF(self, line):
        return True
