#!/usr/bin/env python
# -*- coding:utf-8 -*-

from email.parser import FeedParser
from email.header import decode_header
import sys
import re
import os

PARSER = FeedParser()
# open the mail file
DATA = sys.stdin.read()
PARSER.feed(DATA)
CID_DIR=sys.argv[1]

REPLACE_STUFFS=[]
PARTS_TO_PARSE = [PARSER.close()]

def getValueIgnoreCase(key, dico):
    for dico_key in dico.keys():
        if re.search(key, dico_key, re.I):
            return dico[dico_key]
    return None

while len(PARTS_TO_PARSE) != 0:
    PART = PARTS_TO_PARSE.pop(0)
    if getValueIgnoreCase("content-id", PART):
        assert getValueIgnoreCase('Content-Type', PART)
        NAME = re.search('name="([^"]+)"', getValueIgnoreCase('Content-Type',
                                                              PART), re.I)
        CID = re.search('^<(.+)>$', getValueIgnoreCase('Content-ID', PART), re.I)
        TYPE = re.search('^[^/]+/([^ ;\n\r]+)', getValueIgnoreCase('Content-Type',
                                                                           PART),
                         re.I)
        if NAME:
            NAME = NAME.group(1)
        else:
            NAME = CID.group(1)+ '.' + TYPE.group(1)
        NAME = decode_header(NAME)
        NAME = NAME[0]
        if NAME[1] is not None:
            NAME = NAME[0].decode(NAME[1])
        else:
            NAME = NAME[0]
        # write the attachment into the temp dir
        f = open(os.path.join(CID_DIR, NAME), "wb")
        f.write(PART.get_payload(None, True))
        f.close()
        REPLACE_STUFFS.append(['cid:%s' % (CID.group(1)),
                              NAME,])
    PAYLOAD = PART.get_payload()
    if type(PAYLOAD) == list:
        PARTS_TO_PARSE = PARTS_TO_PARSE + PAYLOAD
print "#!/bin/bash"
print "cat",
for REPLACE_STUFF in REPLACE_STUFFS:
    FROM=REPLACE_STUFF[0]
    TO=REPLACE_STUFF[1]
    print " | sed 's/%s/%s/g'" % (FROM.encode("utf-8"), TO.encode("utf-8")),
