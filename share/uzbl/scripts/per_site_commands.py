#!/usr/bin/env python2
# -*- coding:utf-8 -*-

#Reads the file ${UZBL_DATA_DIR}/commands.conf and tries to match the uri
#to associate a set of commands to launch. The commands file can be specified
#with the UZBL_PER_SITE_COMMANDS_FILE environment variable. The format of the
#commands files in ini. For instance, to provide the commands js alert("bou")
#when the site somesite.com is reached, one would provide the content.
#
#[commands for some site]
#uri_re=somesite.com
#commands=somescript.sh
#
#
#
#The file is parsed from top to bottom. The first match decides list of commands
#to launch.

import os
import sys
import string
import re
import ConfigParser

os.chdir(os.environ["UZBL_DATA_DIR"])

UZBL_URI=sys.argv[1]
COMMANDS_FILE=sys.argv[2]

if not os.path.exists(COMMANDS_FILE):
    print "%s does not exist, no custom command launched" % COMMANDS_FILE
    sys.exit(1)
else:
    print "Reading commands from %s" % COMMANDS_FILE

config = ConfigParser.ConfigParser()
config.optionxform = str    # keys not converted into lower case
config.read(COMMANDS_FILE)
print "Attempt to run custom commands form %s for URI %s" % (COMMANDS_FILE, UZBL_URI)
for section in config.sections():
    items = dict(config.items(section))
    regexp = items["uri_re"]
    script_name_tpl = string.Template(items["script"])
    script_name = script_name_tpl.safe_substitute(os.environ)
    continu = items.get("continue", None)
    if re.search(regexp, UZBL_URI):
        print "Applying section %s" % section
        print "Reading config from %s" % script_name
        with open(script_name, "r") as script_file:
            script = script_file.read()
            with open(os.environ["UZBL_FIFO"], "w") as uzbl_fifo:
                uzbl_fifo.write(script)

        if not continu:
            print "Stop here for this file"
            break
        else:
            print "Continuing the matching"
