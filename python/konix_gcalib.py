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
import dateutil.parser
import konix_collections
import urllib.parse

import logging
logging.basicConfig(level=logging.DEBUG)
LOGGER = logging.getLogger(__file__)

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
                    return
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

PROMPT="GCal({calendar_id}, {extra_query})\n> "

class GCall(cmd.Cmd, object):
    def __init__(self, make_place=False, account=""):
        cmd.Cmd.__init__(self)
        self.db = redis.StrictRedis(decode_responses=True, port=6380)
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
            "status"
        ]
        self._make_place = make_place
        self.account = account

        self.set_prompt()

    @property
    @needs("calendars")
    def calendars(self):
        calendars_string = self.db.get(self.calendars_name)
        if calendars_string:
            calendars = eval(calendars_string)
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
            self.types["Event"]["keys"],
            {k: "" for k in self.types["Event"]["keys"]}
        )

        @property
        def startdate(self):
            if self.start.get("dateTime"):
                return dateutil.parser.parse(self.start.get("dateTime"))
            else:
                return dateutil.parser.parse(
                    self.start.get("date")
                ).replace(tzinfo=datetime.timezone.utc)

        @property
        def enddate(self):
            if self.end.get("dateTime"):
                return dateutil.parser.parse(self.end.get("dateTime"))
            else:
                return dateutil.parser.parse(
                    self.end.get("date")
                ).replace(tzinfo=datetime.timezone.utc)

        @property
        def uid(self):
            """Id that is uniq between recurring events"""
            return re.match(
                "^(?P<id>.+?)(_.{16})?$", self.id).group("id")

        @property
        def duration(self):
            return self.enddate-self.startdate
        Event.startdate = startdate
        Event.enddate = enddate
        Event.duration = duration
        Event.uid = uid

        self.types["CalendarListEntry"]["class"] = CalendarListEntry
        self.types["Event"]["class"] = Event

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
        self.prompt = PROMPT.format(
            calendar_id=self.db.get(self.calendar_id_name),
            extra_query=self.event_list_extra_query
        )

    @property
    def event_list_extra_query(self):
        return self.db.get("event_list_extra_query")

    def do_init(self, line=None):
        self.do_get_user_permission()
        self.do_get_access_token()

    def do_show_event(self, event_id):
        print(self.get_event(event_id))

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
                    "calendars"
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
    def calendars_name(self):
        return self.db_name_transform("calendars")

    def do_clear_user_permission(self, line=None):
        self.db.delete(self.user_code_name)
        self.db.delete(self.device_code_name)

    def do_clear_list_event(self, line=None):
        self.db.delete(self.all_events_name)

    @needs("device_code")
    def get_access_token(self):
        req = urllib.request.Request(
            url='https://accounts.google.com/o/oauth2/token',
            data='client_id={}&client_secret={}&code={}&grant_type=http://oauth.net/grant_type/device/1.0'.format(
                self.client_id,
                self.client_secret,
                self.db.get(self.device_code_name),
            ).encode("utf-8"))
        f = urllib.request.urlopen(req)
        assert f.code == 200
        data = json.loads(f.read().decode("utf-8"))
        self.db.set(self.access_token_name, data["access_token"])
        self.db.expire(self.access_token_name, int(data["expires_in"]))
        self.db.set("token_type", data["token_type"])
        assert self.db.get("token_type") == "Bearer"
        self.db.set(self.refresh_token_name, data["refresh_token"])
        # now that I have a token, the device code is no longer expiring
        self.db.persist(self.device_code_name)

    def get_refresh_token(self):
        req = urllib.request.Request(
            url='https://accounts.google.com/o/oauth2/token',
            data='client_id={}&client_secret={}&refresh_token={}&grant_type=refresh_token'.format(
                self.client_id,
                self.client_secret,
                self.db.get(self.refresh_token_name)
            ).encode("utf-8"))
        f = urllib.request.urlopen(req)
        assert f.code == 200
        data = json.loads(f.read().decode("utf-8"))
        self.db.set(self.access_token_name, data["access_token"])
        self.db.expire(self.access_token_name, int(data["expires_in"]))
        self.db.set("token_type", data["token_type"])
        assert self.db.get("token_type") == "Bearer"

    @lazy("access_token")
    @needs("device_code")
    @provides("access_token")
    def do_get_access_token(self, line=None):
        if self.db.get(self.refresh_token_name):
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
        self.db.set(self.user_code_name, data["user_code"])
        self.db.expire(self.user_code_name, int(data["expires_in"]))
        print("Write the code {}".format(self.db.get(self.user_code_name)))
        os.system("$BROWSER {}".format(self.db.get("verification_url")))
        print("Press Enter when done")
        sys.stdin.readline()
        self.db.set(self.device_code_name, data["device_code"])
        self.db.expire(self.device_code_name, int(data["expires_in"]))

    @provides("calendars")
    def all_calendars(self):
        req = urllib.request.Request(
            url='https://www.googleapis.com/calendar/v3/users/me/calendarList',
            headers={"Authorization": "{} {}".format(
                self.db.get("token_type"),
                self.db.get(self.access_token_name))}
        )
        f = urllib.request.urlopen(req)
        assert f.code == 200
        data = json.loads(f.read().decode("utf-8"))
        calendars = [
            CalendarListEntry(**item)
            for item in data["items"]
        ]
        self.db.set(self.calendars_name, calendars)
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
        f = urllib.request.urlopen(req)
        assert f.code == 200
        data = json.loads(f.read().decode("utf-8"))
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
        events = [
            Event(**i)
            for i in items
            ]
        return events

    @provides("all_events")
    def list_all_events(self):
        calendar_id = self.db.get(self.calendar_id_name)
        if not calendar_id:
            return None
        events = self.get_events(calendar_id, self.event_list_extra_query)
        events.sort(key=lambda event:event.startdate)
        self.db.set(self.all_events_name, events)

    @needs("access_token")
    @needs("all_events")
    @needs("calendar_id")
    def list_events(self, search_terms=""):
        events = eval(self.db.get(self.all_events_name))
        search_terms = shlex.split(search_terms)

        for search_term in search_terms:
            filter_ = eval("lambda x: " + self.event_filter.format(search_term=search_term))
            events = [e for e in events if filter_(e)]

        return events

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
                  sendNotifications=False
              ):
        if make_place is None:
            make_place = self._make_place
        calendar_id = self.db.get(self.calendar_id_name)

        attendees_emails = attendees_emails or []
        sendNotifications = "true" if sendNotifications else "false"
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
            "description" : description,
            "start" : start,
            "end" : end,
            "attendees": attendees,
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
        events = self.get_events(calendar_id, extra_query)
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

    def _update_event(self, calendar_id, event, new_values):
        dict_ = dict(event._asdict())
        dict_ = {
            k: dict_[k]
            for k in dict_
            if k in self.updatable_data
        }
        dict_.update(new_values)
        req = urllib.request.Request(
            url='https://www.googleapis.com/calendar/v3/calendars/{}/events/{}'.format(
                calendar_id,
                event.id
            ),
            data=json.dumps(
                dict_
            ).encode("utf-8"),
            headers={"Content-Type": "application/json",
                     "Authorization": "{} {}".format(
                         self.db.get("token_type"),
                         self.db.get(self.access_token_name))},
            method='PUT'
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
        return self._update_event(calendar_id, event, _dict)

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
