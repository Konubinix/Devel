#!/usr/bin/env python
# -*- coding:utf-8 -*-

import notmuch

db = notmuch.Database(mode=notmuch.Database.MODE.READ_WRITE)
q=notmuch.Query(db, "tag:new and not tag:attachment")
for message in q.search_messages():
    filenames=set([part.get_filename() for part in message.get_message_parts()])
    filenames.remove(None)
    if filenames != set():
        print "Adding the tag attachment to %s" % message
        message.add_tag("attachment")
        message.add_tag("fixed_attachment")
