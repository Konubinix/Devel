#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import re
import getopt
import sys
macro_rep = os.path.join(os.environ['HOME'],".xmacro.d")
current_macro = os.path.join(os.environ['HOME'],".xmacro")
num_regexp = "[^\d]*(\d+).xnl$"
def macro_iter():
    all_files = os.listdir(macro_rep)
    def to_int(f):
        return int(re.match("^(\d+).xnl$",f).group(1))
    all_files.sort(key=to_int)
    for f in all_files:
        yield to_int(f)

def list_macros():
    """returns a list of all macro numbers
    """
    return [m for m in macro_iter()]

def first_macro():
    macros = list_macros()
    if not macros:
        return add_macro()
    else:
        return macros[0]

def add_macro():
    macros = list_macros()
    num = 1
    while num in macros:
        num+=1
    _set_macro(num)
    return num

def say_macro():
    print os.readlink(current_macro)

def assert_check():
    """some routine that must be done before anything else
    """
    if not os.path.isdir(macro_rep):
        os.makedirs(macro_rep)
    if not os.path.isfile(current_macro):
        _set_macro(first_macro())

def _set_macro(num):
    if os.path.isfile(current_macro):
        cur_mac = os.readlink(current_macro)
        cur_num = int(re.match(num_regexp,cur_mac).group(1))
        if cur_num == num:
            return
        else:
            os.unlink(current_macro)
    macro = os.path.join(macro_rep,str(num)+".xnl")
    os.system("ln -sv "+macro+" "+current_macro)
    if not os.path.isfile(macro):
        f = file(macro,"w")
        f.close()

def incr_macro(incr):
    macro_file = os.readlink(current_macro)
    num = int(re.match(num_regexp,macro_file).group(1))
    all_macros = list_macros()
    index = all_macros.index(num)
    index = (index+incr) % len(all_macros)
    _set_macro(all_macros[index])
    return all_macros[index]

def next_macro():
    return incr_macro(1)

def prev_macro():
    return incr_macro(-1)

assert_check()

def usage():
    print "usage : not yet"

if __name__ == "__main__":
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hanps", ["help",])
    except getopt.GetoptError, err:
        # print help information and exit:
        print str(err) # will print something like "option -a not recognized"
        usage()
        sys.exit(2)
    for o, a in opts:
        if o == "-a":
            add_macro()
        elif o == "-n":
            next_macro()
        elif o == "-p":
            prev_macro()
        elif o == "-s":
            say_macro()
        elif o in ("-h", "--help"):
            usage()
            sys.exit()
        else:
            assert False, "unhandled option"
