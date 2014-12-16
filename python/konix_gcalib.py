 #!/usr/bin/env python3
# -*- coding:utf-8 -*-

import cmd
import urllib.request
import json
import os
import re
import sys
import pprint
import redis
import functools
import shlex
import datetime
import time
import collections
import pytz
import parsedatetime

import logging
logging.basicConfig(level=logging.DEBUG)

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

DISCOVERY_URI = 'https://www.googleapis.com/discovery/v1/apis/{api}/{apiVersion}/rest'.format(
                     api="calendar",
                     apiVersion="v3")

EVENT_STRFTIME="%Y-%m-%dT%H:%M:%S.000%z"
EVENT_STRFTIME_ALL_DAY="%Y-%m-%d"

def lazy(expire_key):
    def real_decorator(func):
        @functools.wraps(func)
        def wrapper(self, *args, **kwargs):
            if self.db.get(expire_key):
                logging.debug("{} up to date".format(expire_key))
            else:
                logging.debug("{} out of date (reeval)".format(expire_key))
                return func(self, *args, **kwargs)
        return wrapper
    return real_decorator

PROVIDERS_KEYS = {}

def needs(expire_key):
    def real_decorator(func):
        @functools.wraps(func)
        def wrapper(self, *args, **kwargs):
            if not (
                    self.db.get(expire_key)
                    and expire_key in PROVIDERS_KEYS
            ):
                logging.debug("Calling the provider of the needed key {}".format(expire_key))
                PROVIDERS_KEYS[expire_key](self)
            if self.db.get(expire_key):
                return func(self, *args, **kwargs)
            else:
                logging.error("{} is needed".format(expire_key))
        return wrapper
    return real_decorator

def provides(key):
    def real_decorator(func):
        @functools.wraps(func)
        def wrapper(self, *args, **kwargs):
            return func(self, *args, **kwargs)
        global PROVIDERS_KEYS
        PROVIDERS_KEYS[key] = wrapper
        return wrapper
    return real_decorator

PROMPT="GCal({})> "

class GCall(cmd.Cmd, object):
    def __init__(self):
        cmd.Cmd.__init__(self)
        self.db = redis.StrictRedis(decode_responses=True)
        self.client_id = self.db.get("client_id")
        self.client_secret = self.db.get("client_secret")
        assert self.client_id, "redis-cli set client_id <yourid> (https://console.developers.google.com/project/<yourapp>/apiui/credential)"
        assert self.client_secret, "redis-cli set client_secret <yoursecret> (https://console.developers.google.com/project/<yourapp>/apiui/credential)"
        self.set_prompt()
        calendars_string = self.db.get("calendars")
        self.calendar_filter = "'{search_term}'.lower() in x.summary.lower()"
        self.event_filter = "'{search_term}'.lower() in x.summary.lower()"
        self.calendar_formatter = "str([x.id, x.summary])"
        self.event_formatter = "str([x.id, x.summary])"
        self.get_api()
        self.setup_types()
        if calendars_string:
            self.calendars = eval(calendars_string)
        else:
            self.calendars = []

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
        CalendarListEntry = collections.namedtuple(
            "CalendarListEntry",
            self.types["CalendarListEntry"]["keys"]
        )
        Event = collections.namedtuple(
            "Event",
            self.types["Event"]["keys"]
        )
        self.types["CalendarListEntry"]["class"] = CalendarListEntry
        self.types["Event"]["class"] = Event

    def get_defaultdict(self, keys, dict_):
        res = {k: "" for k in keys}
        res.update(dict_)
        return res

    def make(self, type_, kwargs):
        return self.types[type_]["class"](
            **self.get_defaultdict(
                self.types[type_]["keys"],
                kwargs
            ))

    @property
    @needs("api")
    def api(self):
        return eval(self.db.get("api"))

    @provides("api")
    def get_api(self):
        req = urllib.request.Request(url=DISCOVERY_URI)
        f = urllib.request.urlopen(req)
        assert f.code == 200
        api = json.loads(f.read().decode("utf-8"))
        self.db.set("api", api)

    def set_prompt(self):
        self.prompt = PROMPT.format(self.db.get("calendar_id"))

    def do_init(self, line=None):
        self.do_get_user_permission()
        self.do_get_access_token()

    def do_clear_token(self, line=None):
        self.db.delete("access_token")
        self.db.delete("refresh_token")

    def do_clear_user_permission(self, line=None):
        self.db.delete("user_code")
        self.db.delete("device_code")

    @needs("device_code")
    def get_access_token(self):
        req = urllib.request.Request(
            url='https://accounts.google.com/o/oauth2/token',
            data='client_id={}&client_secret={}&code={}&grant_type=http://oauth.net/grant_type/device/1.0'.format(
                self.client_id,
                self.client_secret,
                self.db.get("device_code"),
            ).encode("utf-8"))
        f = urllib.request.urlopen(req)
        assert f.code == 200
        data = json.loads(f.read().decode("utf-8"))
        self.db.set("access_token", data["access_token"])
        self.db.expire("access_token", int(data["expires_in"]))
        self.db.set("token_type", data["token_type"])
        assert self.db.get("token_type") == "Bearer"
        self.db.set("refresh_token", data["refresh_token"])
        # now that I have a token, the device code is no longer expiring
        self.db.persist("device_code")

    def get_refresh_token(self):
        req = urllib.request.Request(
            url='https://accounts.google.com/o/oauth2/token',
            data='client_id={}&client_secret={}&refresh_token={}&grant_type=refresh_token'.format(
                self.client_id,
                self.client_secret,
                self.db.get("refresh_token")
            ).encode("utf-8"))
        f = urllib.request.urlopen(req)
        assert f.code == 200
        data = json.loads(f.read().decode("utf-8"))
        self.db.set("access_token", data["access_token"])
        self.db.expire("access_token", int(data["expires_in"]))
        self.db.set("token_type", data["token_type"])
        assert self.db.get("token_type") == "Bearer"

    @lazy("access_token")
    @needs("device_code")
    @provides("access_token")
    def do_get_access_token(self, line=None):
        if self.db.get("refresh_token"):
            self.get_refresh_token()
        else:
            self.get_access_token()

    @lazy("device_code")
    @provides("device_code")
    @needs("api")
    def do_get_user_permission(self, line=None):
        api = eval(self.db.get("api"))
        req = urllib.request.Request(
            url='https://accounts.google.com/o/oauth2/device/code',
            data='client_id={}&scope={scopes}'.format(
                self.client_id,
                scopes="+".join(api["auth"]["oauth2"]["scopes"].keys())
            ).encode("utf-8"))
        f = urllib.request.urlopen(req)
        assert f.code == 200
        data = json.loads(f.read().decode("utf-8"))
        self.db.set("verification_url", data["verification_url"])
        self.db.set("interval", data["interval"])
        self.db.set("user_code", data["user_code"])
        self.db.expire("user_code", int(data["expires_in"]))
        print("Write the code {}".format(self.db.get("user_code")))
        os.system("$BROWSER {}".format(self.db.get("verification_url")))
        print("Press Enter when done")
        sys.stdin.readline()
        self.db.set("device_code", data["device_code"])
        self.db.expire("device_code", int(data["expires_in"]))

    @needs("access_token")
    def list_calendars(self, search_term):
        req = urllib.request.Request(
            url='https://www.googleapis.com/calendar/v3/users/me/calendarList',
            headers={"Authorization": "{} {}".format(
                self.db.get("token_type"),
                self.db.get("access_token"))}
        )
        f = urllib.request.urlopen(req)
        if search_term:
            filter_ = eval("lambda x:" + self.calendar_filter.format(search_term=search_term))

        assert f.code == 200
        data = json.loads(f.read().decode("utf-8"))

        self.calendars = [
            cal
            for cal in [
                    self.make("CalendarListEntry", item)
                    for item in data["items"]
            ]
            if not search_term or filter_(cal)
        ]
        self.db.set("calendars", self.calendars)
        return self.calendars

    @needs("access_token")
    def do_list_calendars(self, search_term):
        calendars = self.list_calendars(search_term)
        formatter = eval("lambda x:" + self.calendar_formatter)
        pprint.pprint([formatter(calendar) for calendar in calendars])

    def do__code(self, line=None):
        import readline, rlcompleter
        readline.parse_and_bind("tab: complete")
        import code
        code.interact(local=locals())

    def do_show_calendar(self, calendar_id):
        if not self.calendars:
            print("Run list_calendars first")
            return
        pprint.pprint(*[calendar for calendar in self.calendars if calendar.id == calendar_id])

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
        pprint.pprint(
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
        pprint.pprint(
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

    def do_select_calendar(self, calendar_id):
        if calendar_id:
            self.db.set("calendar_id", calendar_id)
        else:
            self.db.delete("calendar_id")
        self.set_prompt()

    def complete_select_calendar(self, text, line, begidx, endidx):
        return [
            calendar.id for calendar in self.calendars
            if calendar.id.startswith(text)
        ]

    @needs("access_token")
    def list_events(self, search_term):
        calendar_id = self.db.get("calendar_id")
        if not calendar_id:
            return None
        req = urllib.request.Request(
            url='https://www.googleapis.com/calendar/v3/calendars/{}/events'.format(
                calendar_id
            ),
            headers={"Content-Type": "application/json",
                     "Authorization": "{} {}".format(
                self.db.get("token_type"),
                self.db.get("access_token"))}
        )
        f = urllib.request.urlopen(req)
        assert f.code == 200
        data = json.loads(f.read().decode("utf-8"))["items"]
        events = [
            self.make("Event", d)
            for d in data
            ]
        if search_term:
            filter_ = eval("lambda x: " + self.event_filter.format(search_term=search_term))
            events = [e for e in events if filter_(e)]

        return events

    @needs("access_token")
    def do_list_events(self, search_term):
        if not self.db.get("calendar_id"):
            print("Use the command select_calendar first")
            return
        events = self.list_events(search_term)
        formatter = eval("lambda x:" + self.event_formatter)
        pprint.pprint([formatter(event) for event in events])

    @needs("access_token")
    def add_event(self, title, where, when, duration, description="", attendees_emails=None):
        calendar_id = self.db.get("calendar_id")
        attendees_emails = attendees_emails or []
        if not calendar_id:
            print("Use the command select_calendar first")
            return
        calendar = self.get_calendar(calendar_id)
        tz = pytz.timezone(calendar.timeZone)

        cal = parsedatetime.Calendar()
        start, flag = cal.parseDT(when,
                                  sourceTime=datetime.datetime.today(),
                                  tzinfo=tz)
        time_dict = re.match('^\s*((?P<hours>\d+)\s*h)?\s*((?P<minutes>\d+)\s*m)?\s*((?P<seconds>\d+)\s*s)?\s*$',
                             duration).groupdict()
        duration = datetime.timedelta(seconds=int(time_dict['seconds'] or "0"),
                             minutes=int(time_dict['minutes'] or "0"),
                             hours=int(time_dict['hours'] or "0"))
        end = start + duration
        if flag == 2:
            start = {
                "dateTime": start.strftime(EVENT_STRFTIME),
            }
            end = {
                "dateTime": end.strftime(EVENT_STRFTIME),
            }
        else:
            start = {
                "date": start.strftime(EVENT_STRFTIME_ALL_DAY),
            }
            end = {
                "date": end.strftime(EVENT_STRFTIME_ALL_DAY),
            }
        attendees = [{"email": attendee_email} for attendee_email in attendees_emails]
        event = {
            "summary" : title,
            "location" : where,
            "description" : description,
            "start" : start,
            "end" : end,
            "attendees": attendees,
        }

        req = urllib.request.Request(
            url='https://www.googleapis.com/calendar/v3/calendars/{}/events'.format(
                calendar_id
            ),
            data=json.dumps(event).encode("utf-8"),
            headers={"Content-Type": "application/json",
                     "Authorization": "{} {}".format(
                self.db.get("token_type"),
                self.db.get("access_token"))}
        )
        f = urllib.request.urlopen(req)
        assert f.code == 200
        data = json.loads(f.read().decode("utf-8"))
        event = self.make("Event", data)
        return event

    @needs("access_token")
    def do_add_event(self, line):
        """title, where, when, duration, description (optional)"""
        (title, where, when, duration, *description) = shlex.split(line)
        description = description[0] if description else ""
        event = self.add_event(title, where, when, duration, description)
        pprint.pprint(event)

    def do_EOF(self, line):
        return True
