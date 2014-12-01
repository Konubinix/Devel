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

calendar_keys = [
    'accessRole',
    'backgroundColor',
    'colorId',
    'defaultReminders',
    'etag',
    'foregroundColor',
    'id',
    'kind',
    'summary',
    'timeZone',
]
Calendar = collections.namedtuple(
    "Calendar",
    calendar_keys
)
DISCOVERY_URI = 'https://www.googleapis.com/discovery/v1/apis/{api}/{apiVersion}/rest'.format(
                     api="calendar",
                     apiVersion="v3")

DATETIME_FORMAT = "%m/%d/%Y %H:%M"
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
        if calendars_string:
            self.calendars = eval(calendars_string)
        else:
            self.calendars = []

    def get_api(self):
        req = urllib.request.Request(
            url='https://accounts.google.com/o/oauth2/token',
            data='client_id={}&client_secret={}&code={}&grant_type=http://oauth.net/grant_type/device/1.0'.format(
                self.client_id,
                self.client_secret,
                self.db.get("device_code"),
            ).encode("utf-8"))
        f = urllib.request.urlopen(req)
        assert f.code == 200
        self.api = json.loads(f.read().decode("utf-8"))

    def set_prompt(self):
        self.prompt = PROMPT.format(self.db.get("calendar_id"))

    def do_init(self, line=None):
        self.do_get_user_permission()
        self.do_get_access_token()

    def do_clear_token(self, line=None):
        self.db.delete("access_token")

    def do_clear_user_permission(self, line=None):
        self.db.delete("user_code")

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
    def do_get_user_permission(self, line=None):
        req = urllib.request.Request(
            url='https://accounts.google.com/o/oauth2/device/code',
            data='client_id={}&scope=https://www.googleapis.com/auth/calendar+https://www.googleapis.com/auth/calendar.readonly'.format(
                self.client_id,
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
    def do_list_calendars(self, refresh=None):
        if refresh or not self.calendars:
            req = urllib.request.Request(
                url='https://www.googleapis.com/calendar/v3/users/me/calendarList',
                headers={"Authorization": "{} {}".format(
                    self.db.get("token_type"),
                    self.db.get("access_token"))}
            )
            f = urllib.request.urlopen(req)

            assert f.code == 200
            data = json.loads(f.read().decode("utf-8"))
            self.calendars = [
                Calendar(*[item[key] for key in calendar_keys])
                for item in data["items"]
            ]
            self.db.set("calendars", self.calendars)
        pprint.pprint([calendar.id for calendar in self.calendars])

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
    def do_list_events(self, line):
        calendar_id = self.db.get("calendar_id")
        if not calendar_id:
            print("Use the command select_calendar first")
            return
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
        data = json.loads(f.read().decode("utf-8"))
        pprint.pprint(data)

    @needs("access_token")
    def do_add_event(self, line):
        calendar_id = self.db.get("calendar_id")
        if not calendar_id:
            print("Use the command select_calendar first")
            return
        calendar = self.get_calendar(calendar_id)
        tz = pytz.timezone(calendar.timeZone)
        (title, where, when, duration) = shlex.split(line)
        start = datetime.datetime.strptime(when, DATETIME_FORMAT)
        start = tz.localize(start)
        end = start + datetime.timedelta(0, int(duration) * 60)
        event = {
            "summary" : title,
            "location" : where,
            "description" : "",
            "start" : {
                "dateTime": start.strftime("%Y-%m-%dT%H:%M:%S.000%z"),
            },
            "end" : {
                "dateTime": end.strftime("%Y-%m-%dT%H:%M:%S.000%z"),
            },
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
        pprint.pprint(data)

    def do_EOF(self, line):
        return True

if __name__ == "__main__":
    program = GCall()
    if len(sys.argv) > 1:
        program.onecmd(sys.argv[1] + ' "' + '" "'.join(sys.argv[2:]) + '"')
    else:
        program.cmdloop()
