#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import datetime, pytz, time


def get_local_timezone():
    return pytz.timezone(time.tzname[0])


def get_utc_timezone():
    return pytz.timezone("UTC")


def naive_to_local(naive, tz=None):
    tz = tz or get_local_timezone()
    return tz.localize(naive, is_dst=False)


def naive_to_utc(naive):
    tz = get_utc_timezone()
    return tz.localize(naive, is_dst=False)


def date_to_tz(dt, tz=None):
    tz = tz or get_local_timezone()
    return dt.astimezone(tz)


def date_to_utc(dt):
    tz = get_utc_timezone()
    return date_to_tz(dt, tz)


def date_to_naive(dt):
    return dt.replace(tzinfo=None)


def naive_to_local_to_utc_to_naive(naive, tz=None):
    tz = tz or get_local_timezone()
    return date_to_naive(date_to_utc(naive_to_local(naive, tz)))


def naive_to_utc_to_local_to_naive(naive, tz=None):
    tz = tz or get_local_timezone()
    return date_to_naive(date_to_tz(
        naive_to_utc(naive),
        tz,
    ))


def date_to_local(date, tz=None):
    tz = tz or get_local_timezone()
    return date_to_tz(date, tz)


# cf http://stackoverflow.com/questions/4563272/how-to-convert-a-python-utc-datetime-to-a-local-datetime-using-only-python-stand/13287083#13287083
def datetime_naive_insert_local_timezone(datetime_):
    return datetime_.replace(tzinfo=get_utc_timezone()).astimezone(tz=None)
