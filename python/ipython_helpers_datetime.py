#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from datetime import datetime as dt, timedelta as td, timezone as tz

# cf http://stackoverflow.com/questions/4563272/how-to-convert-a-python-utc-datetime-to-a-local-datetime-using-only-python-stand/13287083#13287083
def datetime_naive_insert_local_timezone(datetime_):
    return datetime_.replace(tzinfo=tz.utc).astimezone(tz=None)
