#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from datetime import datetime as dt, timedelta as td

# cf http://stackoverflow.com/questions/4563272/how-to-convert-a-python-utc-datetime-to-a-local-datetime-using-only-python-stand/13287083#13287083
def datetime_naive_insert_local_timezone(datetime_):
    datetime_.replace(tzinfo=datetime.timezone.utc).astimezone(tz=None)
