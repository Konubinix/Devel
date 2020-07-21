#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sys
import notmuch

db = notmuch.Database(mode=notmuch.Database.MODE.READ_WRITE)
q=notmuch.Query(db, " ".join(sys.argv[1:]) if sys.argv[1:] else "tag:new and not tag:attachment")
for message in q.search_messages():
    filenames=set([part.get_filename() for part in message.get_message_parts()])
    try:
        filenames.remove(None)
    except KeyError:
        pass
    if filenames != set():
        if "attachment" not in message.get_tags():
            print("Adding the tag attachment to %s" % message)
            message.add_tag("attachment")
            message.add_tag("fixed_attachment")
