#!/usr/bin/env python3
# -*- coding:utf-8 -*-

# http://stackoverflow.com/questions/5381577/gracefully-recover-from-parse-error-in-expat#5383432
from xmlrpc.client import ExpatParser

import re

# illegal XML 1.0 character ranges
# See http://www.w3.org/TR/REC-xml/#charsets
# I need to make a regular expression substitution on bytes utf-8 data without decoding them. See the comment below.
XML_ILLEGALS = b'|'.join([b'[' + s + b'-' + e + b']' for s, e in [
    ('\u0000'.encode("utf-8"), '\u0008'.encode("utf-8")),             # null and C0 controls
    ('\u000B'.encode("utf-8"), '\u000C'.encode("utf-8")),             # vertical tab and form feed
    ('\u000E'.encode("utf-8"), '\u001F'.encode("utf-8")),             # shift out / shift in
    ('\u007F'.encode("utf-8"), '\u009F'.encode("utf-8")),             # C1 controls
#    ('\uD800'.encode("utf-8"), '\uDFFF'.encode("utf-8")),             # High and Low surrogate areas (it cannot be expressed in utf-8)
    ('\uFDD0'.encode("utf-8"), '\uFDDF'.encode("utf-8")),             # not permitted for interchange
    ('\uFFFE'.encode("utf-8"), '\uFFFF'.encode("utf-8")),             # byte order marks
    ]])

RE_SANITIZE_XML = re.compile(XML_ILLEGALS)

def feed(self, data):
    # filter illegals out
    # It is tempting to decode from utf-8 to unicode to ease the substitution,
    # but since the data is a slice of 1024 characters, it is likely that a
    # multibytes characters (like \xc2\xa0) is split between the locations 1023
    # and 1024. The decoding of the first part (\xc2) would fail in that
    # situation or worse for another multibyte it could find a match for another
    # character and the result would be corrupted. For that reason, the
    # substitution MUST take place in bytes and not str.
    data = RE_SANITIZE_XML.sub(b'', data)
    self._parser.Parse(data, 0)

ExpatParser.feed = feed
