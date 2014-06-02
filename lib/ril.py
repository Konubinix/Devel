#!/usr/bin/env python
# -*- coding:utf-8 -*-

import datetime
import os
from datetime import datetime
import tempfile
import shutil
import glob
import hashlib
import subprocess
import re
import urllib2
# create a html_unescape method to unescape titles
# taken from http://blog.client9.com/2008/10/04/html-unescape-in-python.html
from htmlentitydefs import name2codepoint
def replace_entities(match):
    try:
        ent = match.group(1)
        if ent[0] == "#":
            if ent[1] == 'x' or ent[1] == 'X':
                return unichr(int(ent[2:], 16))
            else:
                return unichr(int(ent[1:], 10))
        return unichr(name2codepoint[ent])
    except:
            return match.group()

entity_re = re.compile(r'&(#?[A-Za-z0-9]+?);')
def html_unescape(data):
    return entity_re.sub(replace_entities, data)

SCRAP=os.environ.get("KONIX_SCRAP_DIR")
RIL=os.environ.get("KONIX_RIL_DIR")
DOWNLOAD_NORMAL_MODE=1
DOWNLOAD_LIGHT_MODE=0
DOWNLOAD_MIRROR_MODE=2
DOWNLOAD_RC_TO_MSG={
        15:"Not need to download, try a better level of download of clean the item",
        16:"Already downloading something about this item",
        0:"Everything went well",
        4:"Wget: Network Failure",
        }
DOWNLOAD_MODE_NAMES={
        DOWNLOAD_NORMAL_MODE:"DOWNLOAD_NORMAL_MODE",
        DOWNLOAD_LIGHT_MODE:"DOWNLOAD_LIGHT_MODE",
        DOWNLOAD_MIRROR_MODE:"DOWNLOAD_MIRROR_MODE",
        }
def download_rc_to_msg(rc):
    return DOWNLOAD_RC_TO_MSG.get(rc, "Unknown error")

def hash(url):
    md5 = hashlib.md5(url)
    return md5.hexdigest()

class RILItem(object):
    def __init__(self, url, stamp=None):
        """If a stamp file already exists in the stamp.txt file, then the stamp value is overriden."""
        # the url is expected to be percent encoded
        url = url.strip()
        match = re.match("^(([^:]+://)?([^?]+)([^#]*))([#].+)?$", url)
        self.url = match.group(1)
        self._index = None
        self._title = None
        self.stamp = stamp
        self._priority = None
        self._read = None
        self.hash = hash(self.url)
        self.dir = os.path.join(
                SCRAP,
                self.hash
                )
        self.url_file_path = os.path.join(
                self.dir,
                "url.txt"
                )
        self.priority_file_path = os.path.join(
                self.dir,
                "priority.txt"
                )
        self.stamp_file_path = os.path.join(
                self.dir,
                "stamp.txt"
                )
        self.index_file_path = os.path.join(
                self.dir,
                "index.txt"
                )
        self.in_dl_lock_file_path = os.path.join(
                self.dir,
                "dl.lock"
                )
        self.dl_mode_file_path = os.path.join(
                self.dir,
                "dl_mode.txt"
                )
        self.wget_log_file_path = os.path.join(
                self.dir,
                "wget.log.txt"
                )
        self.title_file_path = os.path.join(
                self.dir,
                "title.txt"
                )
        self.read_file_path = os.path.join(
                self.dir,
                "read.lock"
                )
        self.update()

    def __hash__(self):
            return int(self.hash, 16)

    def __eq__(self, other):
            return self.hash == other.hash

    def update(self):
        if not os.path.exists(self.dir):
           os.makedirs(self.dir)
        # sanity check that the url is ok
        if os.path.exists(self.url_file_path):
            actualcontent = open(self.url_file_path, "r").read()
            assert actualcontent == self.url, "%s != %s (%s)" % (actualcontent, self.url, self.hash)
        else:
            open(self.url_file_path, "w").write(self.url.encode("utf-8"))

        if os.path.exists(self.stamp_file_path):
            stamp_str = open(self.stamp_file_path, "r").read()
            try:
                self.stamp = float(stamp_str)
            except:
                raise Exception("%s bad stamp" % self.hash)
        elif not self.stamp:
            self.stamp = float(datetime.now().strftime("%s"))
        if not os.path.exists(self.stamp_file_path):
            open(self.stamp_file_path, "w").write(str(self.stamp))

    @property
    def wget_log(self):
        return open(self.wget_log_file_path, "r").read()

    @property
    def priority(self):
        if self._priority:
            pass
        elif os.path.exists(self.priority_file_path):
            self._priority = open(self.priority_file_path, "r").read()
        else:
            self._priority = 'B'
        return self._priority

    @priority.setter
    def priority(self, priority):
        self._priority = priority
        open(self.priority_file_path, "w").write(self._priority)

    @property
    def title(self):
        if self._title:
            pass
        elif os.path.exists(self.title_file_path):
            self._title = open(self.title_file_path, "r").read().decode("utf-8")
        elif self.index and ( self.index.endswith(".pdf") or self.index.endswith(".txt") ):
            self._title = self.index
            open(self.title_file_path, "w").write(self._title.encode("utf-8"))
        elif self.index and os.path.exists(self.index_abs_path) and not self.in_dl:
            title = ""
            beg_found = False
            for line in open(self.index_abs_path, "r"):
                match = re.match("^.*<[Tt][Ii][Tt][Ll][Ee]>(.*)</[Tt][Ii][Tt][Ll][Ee]>.*$", line)
                if match:
                    title = match.group(1)
                    self._title = title
                    break
                if not beg_found:
                    match = re.match("^.*<[Tt][Ii][Tt][Ll][Ee]>(.*)$", line)
                    if match:
                            beg_found = True
                            title = match.group(1)
                            continue
                else:
                    match = re.match("^(.*)</[Tt][Ii][Tt][Ll][Ee]>.*$", line)
                    if match:
                        title += match.group(1)
                        self._title = title.replace("\n", " - ").replace("\r", " - ")
                        break
                    else:
                        title += line
            if self._title:
                self._title = self._title.strip()
                try:
                    self._title = self._title.decode("utf-8")
                except:
                    self._title = self._title.decode("latin-1")
            else:
                self._title = self.index
            open(self.title_file_path, "w").write(self._title.encode("utf-8"))
        if self._title:
            return self._title
        else:
            return self.url

    @property
    def title_html_unescaped(self):
        return html_unescape(self.title)

    @property
    def index(self):
        if self._index and not os.path.exists(self.index_file_path):
            self._index = None
        if self._index:
            pass
        elif os.path.exists(self.index_file_path):
            self._index = open(self.index_file_path, "r").read().decode("utf-8")
        elif os.path.exists(self.wget_log_file_path):
            for line in open(self.wget_log_file_path, "r").readlines():
                match = re.match("^Saving to: ‘(.+)’$", line)
                if match:
                    self._index = match.group(1).decode('utf-8')
                    break
            if self._index:
                open(self.index_file_path, "w").write(self._index.encode("utf-8"))
        return self._index

    @property
    def index_url(self):
        return self.index.replace("%", "%25")

    @property
    def index_abs_path(self):
        index = self.index
        if index:
            return os.path.join(
                self.dir,
                self.index)
        else:
            return None

    @staticmethod
    def from_url_path(url_path):
        content = open(url_path, "r").read().decode("utf-8")
        assert content != ""
        return RILItem(content, os.stat(url_path).st_atime)

    @staticmethod
    def from_directory(dir):
        urlpath = os.path.join(
                dir,
                "url.txt"
                )
        return RILItem.from_url_path(urlpath)

    @staticmethod
    def from_hash(hash):
        files = glob.glob(
            os.path.join(
                SCRAP,
                hash
                ) + "*"
                )
        assert len(files) == 1
        dir = files[0]
        return RILItem.from_directory(dir)

    @property
    def read(self):
        if self._read == None:
            self._read = os.path.exists(self.read_file_path)
        return self._read

    @read.setter
    def read(self, value):
        self._read = value
        if self._read:
            open(self.read_file_path, "w").write("1")
        else:
            os.unlink(self.read_file_path)

    def remove(self):
        shutil.rmtree(self.dir)

    def clean(self):
        self.remove()
        os.makedirs(self.dir)
        open(self.url_file_path, "w").write(self.url.encode("utf-8"))
        open(self.stamp_file_path, "w").write(str(self.stamp))

    def download(self, mode=DOWNLOAD_NORMAL_MODE):
        if mode <= self.last_dl_mode and self.dled:
                return 15
        elif self.in_dl:
            return 16
        self.in_dl = True
        command = ["wget",
                   "--unlink",
                   "--convert-links",
                   "--force-directories",
                   "--adjust-extension",
                   "--span-hosts",
                   "--no-iri",
                   "--restrict-file-names=windows",
                 ]
        cookie_file = os.environ.get("UZBL_COOKIES_FILE",
                                     None)
        session_cookie_file = os.environ.get("UZBL_COOKIES_FILE",
                                     None)
        if cookie_file and os.path.exists(cookie_file):
          command += ["--load-cookies", cookie_file,]
        if session_cookie_file and os.path.exists(session_cookie_file):
          command += ["--load-cookies", session_cookie_file,]

        mode_to_attributes = {
                DOWNLOAD_LIGHT_MODE : [],
                DOWNLOAD_NORMAL_MODE : ["--page-requisites"],
                DOWNLOAD_MIRROR_MODE : ["--page-requisites", "--mirror"],
                }
        command += mode_to_attributes[mode]
        command += [self.url]
        if os.path.exists(self.wget_log_file_path):
            os.unlink(self.wget_log_file_path)
        output = open(self.wget_log_file_path, "w")
        output.write("Command: " + " ".join(command) + "\n\n")
        output.flush()
        p = subprocess.Popen(command,
                cwd=self.dir,
                stdout=output,
                stderr=output)
        p.wait()
        output.write("\nreturncode: "+ str(p.returncode))
        output.close()
        if os.path.exists(self.dl_mode_file_path):
            os.unlink(self.dl_mode_file_path)
        open(self.dl_mode_file_path, "w").write(str(mode))
        self.in_dl = False
        assert self.in_dl == False
        return p.returncode

    @property
    def last_dl_mode(self):
        if os.path.exists(self.dl_mode_file_path):
            try:
                return int(open(self.dl_mode_file_path, "r").read())
            except:
                return None
        else:
            return None

    @property
    def formatted_stamp(self):
        stamp = datetime.fromtimestamp(self.stamp)
        return stamp.strftime("%y/%m/%d-%H:%M:%S")

    @property
    def dled(self):
        return self.index != None

    @property
    def in_dl(self):
        return os.path.exists(
                self.in_dl_lock_file_path
                )

    @in_dl.setter
    def in_dl(self, value):
        if value:
            open(self.in_dl_lock_file_path, "w").write("1")
        elif self.in_dl:
            os.unlink(self.in_dl_lock_file_path)

    def __str__(self):
        info = self.title_html_unescaped
        if not info:
            info = self.url
        info = info
        if self.in_dl:
            in_dl = "[DOWNLOADING] "
        else:
            in_dl = ""
        if self.read:
            read_str = "read"
        else:
            read_str = "unread"
        if self.dled:
            dl_mode = self.last_dl_mode
        else:
            dl_mode = "notdled"
        res = u"%s:%s\n%s%s (%s, %s)\n%s" % (
                self.priority,
                info,
                in_dl,
                self.hash[:8],
                read_str,
                dl_mode,
                self.formatted_stamp,
                )
        res = res.encode("utf-8")
        return res

    def __repr__(self):
        return "RILItem: " + self.hash

    def open(self):
        assert self.dled
        os.system('mimeopen "%s"' % self.index_abs_path.encode("utf-8").replace('"', '\\"'))

    def open_url(self):
        os.system('"%s" "%s"' % (os.environ["BROWSER"], self.url))

class RILItems(object):
    def __init__(self, items=None):
        if items == None:
            items = set()
        # use a set to have uniq ril items
        self.items = items

    def add_from_scrap_directory(self, directory):
        urls = glob.glob(directory + "/*/url.txt")
        for url_path in urls:
            item = RILItem.from_url_path(url_path)
            # sanity check
            new_url_path = item.url_file_path
            assert new_url_path == url_path, "%s should be deleted since it is a dupliate of %s (%s vs %s)" % (
                        url_path,
                        new_url_path,
                        open(url_path, "r").read(),
                        open(new_url_path, "r").read()
                        )
            self.items.add(item)

    def add_from_ril_directory(self, ril_directory):
        ril_url_paths = glob.glob(ril_directory + "/*txt")
        self.items.update([RILItem.from_url_path(url_path) for url_path in ril_url_paths])
        # no error nor assertion, then remove ril files
        for path in ril_url_paths:
            os.unlink(path)

    def items_sorted(self, key, reverse=False):
        items = list(self.items)
        #import pdb
        #pdb.set_trace()
        items.sort(key=key, reverse=reverse)
        return items

    @property
    def items_stamp_sorted(self):
        return self.items_sorted(key=lambda x:x.stamp, reverse=True)

    @property
    def items_priority_stamp_sorted(self):
        items = self.items_sorted(key=lambda x:x.stamp, reverse=True)
        items.sort(key=lambda x:x.priority)
        return items

def get_rilitems():
    rilitems = RILItems()
    rilitems.add_from_scrap_directory(SCRAP)
    ril_prehook = os.path.join(RIL, "prehook.sh")
    if os.path.exists(ril_prehook):
        assert 0 == os.system(ril_prehook)
    rilitems.add_from_ril_directory(RIL)
    return rilitems

def rilitems_stamp_sorted():
    rilitems = get_rilitems()
    return rilitems.items_stamp_sorted

def rilitems_priority_stamp_sorted():
    rilitems = get_rilitems()
    return rilitems.items_priority_stamp_sorted
