#!/usr/bin/env python

## Filename: notmuch_addresses.py
## Copyright (C) 2010-11 Jesse Rosenthal
## Author: Jesse Rosenthal <jrosenthal@jhu.edu>

## This file is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published
## by the Free Software Foundation; either version 2, or (at your
## option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## NOTE: This script requires the notmuch python bindings.

import notmuch
import ConfigParser
import optparse
import email.utils
import os.path
import sys
import re

# email.utils.parseaddr() is very slow for some reason. I'm still
# using it, though, but I'm considering whether it's necessary. This
# trivial replacement might be missing important
# functionality. Sebastian Spaeth fixed up a problem here.
def my_parseaddr(addr):
    parsed = re.split(r'[<>]', addr)
    if len(parsed) == 1:
        return ('', parsed[0].strip())
    else:
        return (parsed[0].strip("\"\t "), parsed[1].strip())


class EmailsWithNames(object):
    """A collection of email addresses, each with an arbitrary amount
    of associated real names. This class can return the best-choice
    real name for a given email address (based on frequency) as well
    as a list email addresses (with or without best-choice real
    names), sorted by frequency.
    """

    def __init__(self):
        self.emails = {}

    def add_email_and_name(self, email, real_name):
        if email in self.emails:
            if real_name in self.emails[email]:
                self.emails[email][real_name] += 1
            else:
                self.emails[email][real_name] = 1
        else:
            self.emails[email] = {real_name:1}

    def email_freq(self, email):
        if email in self.emails:
            return sum(self.emails[email].values())
        else:
            return 0

    def name_freq(self, email, real_name):
        if email in self.emails:
            if real_name in self.emails[email]:
                return self.emails[email][real_name]
            else:
                return 0
        else:
            raise Exception

    def assoc_name(self, email):
        names = self.emails[email].keys()
        names.sort(key=lambda(name): self.name_freq(email, name),
                   reverse=True)
        # We don't want to return an empty name if we can help it, so
        # first we check to see if the most frequent name is the empty
        # string.
        #
        # If it's not empty, cool.
        if len(names[0]) > 0:
            return names[0]
        # If so, we check to see if it's the only possibility.
        else:
            # If it is, we're stuck with it...
            if len(names) == 1:
                return names[0]
            # ...but if not, we can go with the second option.
            else:
                return names[1]

    def sorted_email_list(self):
        return sorted(self.emails.keys(),
                      key=self.email_freq,
                      reverse=True)

    def sorted_email_and_names_list(self):
        email_list = self.sorted_email_list()
        return [email.utils.formataddr((self.assoc_name(e), e))
                for e in email_list]

class NotmuchAddressMatcher(object):
    """A simple address matcher, based on information information from
    the user's $HOME/.notmuch-config file.
    """

    def __init__(self, query_name, match_function=None):
        """
        """
        config = ConfigParser.ConfigParser()
        config.read(os.path.expanduser(
            os.environ.get("NOTMUCH_CONFIG","~/.notmuch-config")))
        self.db_path = config.get("database", "path")
        self.email = config.get("user", "primary_email")
        try:
            other_emails=config.get("user", "other_email").split(";")
            self.other_emails=[addr.strip() for addr in other_emails if addr]
        except ConfigParser.NoOptionError:
            self.other_emails = []

        self.query_name = query_name
        if match_function:
            self.match_function = match_function(self.query_name)
        else:
            self.match_function = self.trivial_match_function(self.query_name)
        self.matches = []

    def trivial_match_function(self, name):
        """ This outputs a trivial matching function (case
        independent, same starting letters). More sophisticated ones
        could be developed. It is the default match function, but can
        be overwritten by the user.
        """
        def output (x):
            return x.lower().startswith(name.lower())
        return output


    def _get_matching_messages(self):
        # Credit to Sebastian Spaeth for figuring out how to query
        # notmuch in a more efficient way.

        notmuch_db = notmuch.Database(self.db_path)
        query_string = "(from:" + self.email

        for addr in self.other_emails:
            query_string += (" OR from:" + addr)

        query_string += ") and to:" + self.query_name + "*"

        query = notmuch.Query(notmuch_db, query_string)
        return query.search_messages()


    def generate_matches(self):
        msgs = self._get_matching_messages()
        emails = EmailsWithNames()
        for m in msgs:
            addrs = []
            for h in ('to', 'cc', 'bcc'):
                v = m.get_header(h)
                if v > '':
                    addrs.append(v)
                    parsed_addrs = email.utils.getaddresses(addrs)
            for addr in parsed_addrs:
                mail = addr[1].lower()
                split_names = addr[0].split(" ")
                if (len([name for name in split_names
                         if self.match_function(name)]) > 0
                    or
                    self.match_function(mail)):

                    emails.add_email_and_name(mail, addr[0])

        self.matches = emails.sorted_email_and_names_list()

if __name__ == '__main__':

    if len(sys.argv) < 2:
        print "You must enter a name query"
    else:
        name = " ".join(sys.argv[1:])
        matcher = NotmuchAddressMatcher(name)
        matcher.generate_matches()

        for elem in matcher.matches: print (elem)
