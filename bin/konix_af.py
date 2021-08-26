#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import html2text
import urllib2, urllib
import logging
import re
import os
import sys
import pickle
logging.basicConfig(level=logging.DEBUG)
from BeautifulSoup import BeautifulSoup
import argparse

parser = argparse.ArgumentParser(
    description="AF.",
)

parser.add_argument('--no-cache', action="store_true", default=False,
                    help="No cache")

parser.add_argument('-i', '--get-session-id', action="store_true", default=False,
                    help="Get session id and quit")

parser.add_argument('words', metavar='word', type=str, nargs='*',
                   help='the words')

args = parser.parse_args()

def get_session_id():
    url = "http://atilf.atilf.fr/Dendien/scripts/generic/showps.exe?p=main.txt;host=interface_academie9.txt;java=no;"
    logging.debug("Getting url " + url)
    r = urllib2.Request(url)
    res = urllib2.urlopen(r)
    assert res.code == 200
    soup = BeautifulSoup(res.read())
    # go to the "Menu principal" button page
    action = soup.find("form")["action"]
    # extract the session number from it
    match = re.match(".+s=(.+);.+", action)
    assert match
    return match.group(1)

if args.get_session_id:
    print get_session_id()
    sys.exit(0)

request = " ".join(args.words)
logging.debug("Request is %s" % request)

results_cache = {}
if not args.no_cache:
    cache_path = os.environ.get("AF_CACHE", None)
    assert cache_path
    if os.path.exists(cache_path):
        with open(cache_path, "r") as fi:
            results_cache = pickle.load(fi)

results = results_cache.get(request, None)
if results is not None:
    logging.debug("Got results from the cache")
else:
    session_number = get_session_id()

    # url = "http://atilf.atilf.fr" + action
    # logging.debug("Getting url " + url)
    # r = urllib2.Request(url)
    # res = urllib2.urlopen(r)
    # assert res.code == 200
    # soup = BeautifulSoup(res.read())
    # # go to the "Lancer une recherche dans le dictionnaire" page
    # research_page = soup.find(attrs={'href' : re.compile("form.exe")})["href"]

    # url = "http://atilf.atilf.fr" + research_page
    # logging.debug("Getting url " + url)
    # r = urllib2.Request(url)
    # res = urllib2.urlopen(r)
    # assert res.code == 200
    # soup = BeautifulSoup(res.read())
    # # launch the search
    # research_page = soup.find("form", attrs={"action" :
    #                                          re.compile("cherche.exe")})["action"]
    params = urllib.urlencode(
        {
            "var0" : request,
            "var2" : "",
            "var9" : "*!!*",
            "var11" : "*!!*",
            "var15" : "",
        }
    )

    #url = "http://atilf.atilf.fr" + research_page
    url = "http://atilf.atilf.fr/dendien/scripts/generic/cherche.exe?15;s=%s;;" % session_number
    logging.debug("Getting url " + url)
    logging.debug("  with params " + params)
    r = urllib2.Request(url, params)
    res = urllib2.urlopen(r, params)
    assert res.code == 200
    soup = BeautifulSoup(res.read())

    # go to the affiche page
    affiche_page = soup.find("frame", attrs={"src": re.compile("affiche.exe")})["src"]

    url = "http://atilf.atilf.fr" + affiche_page
    logging.debug("Getting url " + url)
    r = urllib2.Request(url)
    res = urllib2.urlopen(r)
    assert res.code == 200
    soup = BeautifulSoup(res.read())

    # print all the results
    results = [unicode(result) for result in soup.findAll("td")]

    results_cache[request] = results
    if not args.no_cache:
        cache_path = os.environ.get("AF_CACHE", None)
        assert cache_path
        with open(cache_path, "w") as fi:
            pickle.dump(results_cache, fi)

if len(results) == 0:
    print "No result"
else:
    print "Found %s results" % len(results)
    for result in results:
        print "############################"

        print html2text.html2text(result)
