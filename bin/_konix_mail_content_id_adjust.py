#!/usr/bin/env python
# -*- coding:utf-8 -*-

from email.parser import FeedParser
import sys
import re

PARSER = FeedParser()
# open the mail file
DATA = sys.stdin.read()
PARSER.feed(DATA)

REPLACE_STUFFS=[]
PARTS_TO_PARSE = [PARSER.close()]
while len(PARTS_TO_PARSE) != 0:
    PART = PARTS_TO_PARSE.pop(0)
    if "Content-ID" in PART.keys():
        assert 'Content-Type' in PART.keys()
        NAME = re.search('name="([^"]+)"', PART['Content-Type'])
        CID = re.search('^<(.+)>$', PART['Content-ID'])
        assert NAME
        REPLACE_STUFFS.append(['cid:%s' % (CID.group(1)),
                              NAME.group(1),])
    PAYLOAD = PART.get_payload()
    if type(PAYLOAD) == list:
        PARTS_TO_PARSE = PARTS_TO_PARSE + PAYLOAD
for REPLACE_STUFF in REPLACE_STUFFS:
    FROM=REPLACE_STUFF[0]
    TO=REPLACE_STUFF[1]
    sys.stderr.write(FROM+" "+TO+"\n")
    DATA = re.sub(FROM, TO, DATA)
print DATA
