#!/usr/bin/env python
# -*- coding:utf-8 -*-

# http://stackoverflow.com/questions/5381577/gracefully-recover-from-parse-error-in-expat#5383432
from xmlrpc.client import ExpatParser

import re

# illegal XML 1.0 character ranges
# See http://www.w3.org/TR/REC-xml/#charsets
XML_ILLEGALS = u'|'.join(u'[%s-%s]' % (s, e) for s, e in [
    (u'\u0000', u'\u0008'),             # null and C0 controls
    (u'\u000B', u'\u000C'),             # vertical tab and form feed
    (u'\u000E', u'\u001F'),             # shift out / shift in
    (u'\u007F', u'\u009F'),             # C1 controls
    (u'\uD800', u'\uDFFF'),             # High and Low surrogate areas
    (u'\uFDD0', u'\uFDDF'),             # not permitted for interchange
    (u'\uFFFE', u'\uFFFF'),             # byte order marks
    ])

RE_SANITIZE_XML = re.compile(XML_ILLEGALS, re.M | re.U)

def feed(self, data):
    # decode, filter illegals out, then encode back to utf-8
    data = RE_SANITIZE_XML.sub('', data.decode("utf-8")).encode('utf-8')
    self._parser.Parse(data, 0)

ExpatParser.feed = feed
