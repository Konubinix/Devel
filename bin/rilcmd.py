#! /usr/bin/env python3

import cmd
import ril
import sys
import urllib2
import shlex
import os
import re
import readline
# handle the history
histfile = os.environ.get("RILCMD_HISTORY",
                          os.path.expanduser("~/.rilcmd_history"))
try:
    readline.read_history_file(histfile)
except IOError:
    pass
import atexit
atexit.register(readline.write_history_file, histfile)
del histfile

def str2int(string):
    if string == "":
        return None
    else:
        return int(string)

class RilCmd(cmd.Cmd, object):
    def __init__(self, *args, **kwargs):
        super(RilCmd, self).__init__(*args, **kwargs)
        self.__count = 0

    def do_count(self, line):
        if line == "":
            print self.__count
        else:
            self.__count = int(line)
            print "Set count to %s" % self.__count

    def do_EOF(self, line):
        print "Bye"
        sys.exit(0)

    def do_ipython(self, line=None):
        import IPython
        IPython.start_ipython(argv=[], user_ns=locals())

    def do_search_x(self, line):
        """Show only items for which the expression is True. x is an Item. See
        help_rilitem for more information about what is available."""
        items = self.get_items()
        self.filter_x(items, line)

    def do_help_rilitem(self, line):
        """Show the help about rilitem."""
        help(ril.RILItem)

    def do_filter_x(self, line):
        items = self.printed_items
        self.filter_x(items, line)

    def do_search_title(self, line):
        items = self.get_items(None, None)
        items = [item for item in items if re.match('^.*' +line+'.*$', item.title_html_unescaped, re.I)]
        self.print_items(items)

    def do_ls_read(self, line):
        items = self.get_items(str2int(line), True)
        self.print_items(items)

    def do_ls(self, line):
        items = self.get_items(str2int(line), False)
        self.print_items(items)

    def do_rels(self, line):
        items = self.printed_items
        if line != "":
            items = items[:str2int(line)]
        self.print_items(items)

    def do_lsdl(self, line):
        items = self.get_items(str2int(line), False, True)
        self.print_items(items)

    def do_rm(self, line):
        item = self.get_item(line)
        info = item.url
        item.remove()
        print "Removed %s" % info

    def do_toggle_read(self, line):
        item = self.get_item(line)
        item.read = not item.read
        print "%s read state set to %s" % (item, item.read)

    def do_add(self, url):
        item = ril.RILItem(url)
        print "Created item %s" % item

    def do_describe(self, line):
        item = self.get_item(line)
        print item
        print """Url   : %s""" % item.url
        print """Index : %s""" % unicode(item.index).encode("utf-8")

    def do_open(self, line):
        item = self.get_item(line)
        self.open(item)

    def do_open_url(self, line):
        item = self.get_item(line)
        item.open_url()

    def do_show_wget_log(self, line):
        item = self.get_item(line)
        print item.wget_log

    def do_explorer(self, line):
        item = self.get_item(line)
        os.system("mimeopen '%s'" % item.dir)

    def do_open_last_unread(self, line):
        self.open(self.get_items(1, False)[0])

    def do_dl(self, line):
        line = shlex.split(line)
        assert len(line) < 3
        if len(line) > 0:
            info = line[0]
        else:
            info = "0"
        if len(line) > 1:
            dl_mode = line[1]
        else:
            dl_mode = 0
        item = self.get_item(info)
        self.dl_item(item, dl_mode)

    def do_dl_all(self, line):
        "dl_all DOWNLOAD_MODE (default to DOWNLOAD_NORMAL_MODE)"
        if line == '':
            line = ril.DOWNLOAD_NORMAL_MODE
        dl_mode = int(line)
        items = self.get_items(None, False)
        ok = 0
        ko = 0
        ig = 0
        for item in items:
            res = self.dl_item(item, dl_mode)
            if res in (15, 16):
                ig += 1
            elif res != 0:
                ko += 1
            else:
                ok += 1
        print "----- In brief"
        print "OK      : " + str(ok)
        print "KO      : " + str(ko)
        print "IGNORED : " + str(ig)

    def do_clean(self, line):
        item = self.get_item(line)
        item.clean()
        print "Cleaned %s" % item

    def do_clean_all(self, line):
        items = self.get_items(None, False)
        for item in items:
            item.clean()
            print "Cleaned %s" % item

    def do_clean_printed_items(self, line):
        assert self.printed_items
        items = self.printed_items
        for item in items:
            item.clean()
            print "Cleaned %s" % item

    def do_reset(self, line):
        self.printed_items = None
        print "Reseted printed items"

    # Helpers
    def filter_x(self, items, line):
        fil = eval("lambda x:" + line)
        items = [item for item in items if fil(item)]
        self.print_items(items)

    def dl_item(self, item, dl_mode):
        print "Dling %s in mode %s" % (
                    item,
                    ril.DOWNLOAD_MODE_NAMES[int(dl_mode)],
                    )
        res = item.download(int(dl_mode))
        print ril.DOWNLOAD_RC_TO_MSG.get(res, "Unknown error")
        return res

    def open(self, item):
        if not item.dled:
            print "Cannot open %s since it has not been dled yet" % item
            return
        else:
            print "Opening %s" % item
            item.open()

    def print_items(self, items):
        self.printed_items = items
        number = 0
        for item in items:
            print number, item
            number += 1
            if self.__count != 0 and number >= self.__count:
                print "-- Stopped (set count to display more)"
                return

    def get_items(self, number=None, read=None, in_dl=None):
        items = ril.rilitems_priority_stamp_sorted()
        if read is not None:
            items = [item for item in items if item.read == read]
        if in_dl is not None:
            items = [item for item in items if item.in_dl == in_dl]
        if number is not None:
            items = items[:number]
        return items

    def get_item_printed(self, number):
        assert self.printed_items
        return self.printed_items[number]

    def get_item_hash(self, hash=""):
        assert hash and hash != ""
        items = [item for item in self.get_items() if item.hash.startswith(hash)]
        assert len(items) == 1
        return items[0]

    def get_item(self, req):
        if req.startswith("."):
            return self.get_item_hash(req[1:])
        else:
            return self.get_item_printed(int(req))

if __name__ == "__main__":
    c = RilCmd()
    c.cmdloop()

# Local Variables:
# python-indent: 4
# End:
