#! /usr/bin/env python

import os
from sys import argv
from urlparse import urlparse

def xdghome(key, default):
    '''Attempts to use the environ XDG_*_HOME paths if they exist otherwise
    use $HOME and the default path.'''

    xdgkey = "XDG_%s_HOME" % key
    if xdgkey in os.environ.keys() and os.environ[xdgkey]:
        return os.environ[xdgkey]

    return os.path.join(os.environ['HOME'], default)

# Setup xdg paths.
DATA_DIR = os.path.join(xdghome('DATA', '.local/share/'), 'uzbl/')

# Blockfile location.
BLOCKFILE = os.path.join(DATA_DIR, 'adblock')

JAVASCRIPT = ' '.join(filter(None, map(str.strip, '''
var uzblAdBlock = function() {
    var toblock = %s;
    for(var n = 0; n < toblock.length; n++) {
        var items;
        while (1) {
            try {
                items = document.evaluate(toblock[n], document, null, XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
                if (items == null) { break; }
                var i = items.iterateNext();
                if (i == null) { break; }
                i.parentNode.removeChild(i);
            } catch (e) {
                break;
            }
        }
    }
};
'''.split('\n'))))


def get_domain(url):
    '''Return domain segment of url.'''

    if not url.startswith('http'):
        url = "http://%s" % url

    loc = urlparse(url).netloc
    if loc.startswith('www.'):
        loc = loc[4:]

    return loc


def adblock(url, fifo):
    fh = open(BLOCKFILE, 'r')
    lines = [line.strip() for line in fh.readlines()]
    fh.close()

    rules, capture = [], False
    for l in lines:
        if not l: # newline splits section
            capture = False

        elif l[0] == '#':
            continue

        elif capture:
            rules.append(l)

        elif l[-1] == ':':
            if get_domain(l[:-1]) == url or l[:-1] == "global":
                capture = True

    rulestr = repr(rules).replace("@", "\@")
    js = "js %s\n" % (JAVASCRIPT % rulestr)
    fh = open(fifo, "w")
    fh.write(js)
    fh.close()

if __name__ == '__main__':
    adblock(get_domain(os.environ['UZBL_URI']), os.environ['UZBL_FIFO'])
