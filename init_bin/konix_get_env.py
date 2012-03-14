#!/usr/bin/env python
# -*- coding:utf-8 -*-

import sys
import os
import ConfigParser
import string
import re
import logging
import socket
import random

logging.basicConfig(level=logging.DEBUG)
default_config_file = os.path.join(os.path.expanduser("~"),".env_"+os.sys.platform+".conf")
DEFAULT_CONFIG={
    'KONIX_PWD' : os.getcwd()
    }

ENV_BACKUP_FILE_TEMPLATE=os.path.join(os.path.expanduser("~"),".env_backup_%s_"+os.sys.platform+".conf")

# by default, get the values from os.environ
DEFAULT_ENVIRON = os.environ

def mergeItemsOfSection(items):
    new_items = {}
    for key,value in items:
        new_value = value.replace(",",os.pathsep)
        if new_items.get(key) == None:
            new_items[key] = new_value
        else:
            new_items[key] = os.pathsep.join((new_items[key],new_value,))
    return new_items

def getConfigFromEnvFile(envfile, previous_config):
    if not os.path.exists(envfile):
        print >>sys.stderr,"Config file",envfile,"does not exist"
        return previous_config
    config = ConfigParser.ConfigParser()
    config.optionxform = str    # keys not converted into lower case
    config.read(envfile)
    new_config = previous_config
    for section in config.sections():
        assert(section in ("prefix","replace","suffix",))
        items = mergeItemsOfSection(config.items(section))
        for key in items.keys():
            value = items[key]
            env_value = DEFAULT_ENVIRON.get(key)
            if env_value == None:
                env_value = ""

            value = value.replace('\n','')
            substitute_config = {}
            substitute_config.update(DEFAULT_CONFIG)
            substitute_config.update(DEFAULT_ENVIRON)
            substitute_config.update(previous_config)
            value_template = string.Template(value)
            value = value_template.safe_substitute(substitute_config)

            #env_value = env_value.replace("\\","\\\\")
            #value = value.replace('\\','\\\\')

            # the previous value is either set by a previous step in
            # the loop
            if new_config.get(key) != None:
                previous_value = new_config[key]
            # or got from the env_value
            else:
                previous_value = env_value

            logging.debug("analysing ["+key+"] = "+value+" (previous was "+previous_value+")")
            if section == "replace" or previous_value == "":
                logging.debug("Replace previous value")
                new_config[key] = value
            elif section == "prefix":
                if not previous_value.startswith(value):
                    new_config[key] = "".join((value,previous_value,))
                    logging.debug("Prepending to previous value to get "+new_config[key])
                else:
                    new_config[key] = previous_value
                    logging.debug("Not prepended")
            elif section == "suffix":
                if not previous_value.endswith(value):
                    new_config[key] = "".join((previous_value,value,))
                    logging.debug("Appending to previous value to get "+new_config[key])
                else:
                    new_config[key] = previous_value
                    logging.debug("Not appended")
            else:
                assert(False)


    return new_config

def getBackupEnvFileName(stamp):
    return ENV_BACKUP_FILE_TEMPLATE % stamp

def createBackupEnvFile(stamp):
    env_backup_file_name = getBackupEnvFileName(stamp)
    if os.path.exists(env_backup_file_name):
        os.remove(env_backup_file_name)
    f = open(env_backup_file_name, "w")
    f.write("[replace]\n")
    for item in os.environ.items():
        f.write(item[0]+"="+item[1]+"\n")
    f.close()

def createBackupFileOrUseIt(stamp):
    backup_file = None
    if not stamp or not os.path.exists(getBackupEnvFileName(stamp)):
        stamp = str(random.randint(0,300000))
        logging.debug("Creating new backup env file with stamp "+stamp)
        createBackupEnvFile(stamp)
    else:
        logging.debug("Using backup env file with stamp "+stamp)
        backup_file = getBackupEnvFileName(stamp)
    return (stamp, backup_file)

def main():
    config = {}
    GIVEN_CONFIG_FILE = None
    if len(sys.argv) > 1:
        GIVEN_CONFIG_FILE = sys.argv[1]
        if not os.path.exists(GIVEN_CONFIG_FILE):
            logging.warning("The file given as argument was not found, ignoring it : "+GIVEN_CONFIG_FILE)
            GIVEN_CONFIG_FILE=None
    if GIVEN_CONFIG_FILE:
        config = getConfigFromEnvFile(GIVEN_CONFIG_FILE, config)
    else:
        config = getConfigFromEnvFile(default_config_file,config) # get the default config to know the platform

        # changing of platform must force a new env to be loaded. Then the env loading is done if
        # - the platforms are differents
        # or
        # - env_done is not set to 1
        logging.debug("the config is for "+config["KONIX_PLATFORM"]+", the environ says "+os.environ.get("KONIX_PLATFORM", "nothing"))
        if config["KONIX_PLATFORM"] != os.environ.get("KONIX_PLATFORM") or os.environ.get("KONIX_ENV_DONE") != "1":
            # ####################################################################################################
            # Hardcoded ones
            # ####################################################################################################
            config["HOSTNAME"] = socket.gethostname()
            (stamp, backup_file) = createBackupFileOrUseIt(os.environ.get("KONIX_ENV_STAMP", None))
            config["KONIX_ENV_STAMP"] = stamp
            logging.debug("Parsing the env")
            devel_dir = config["KONIX_DEVEL_DIR"]
            konix_config = os.path.join(devel_dir,"config")
            config_file = os.path.join(konix_config, "env.conf")
            config_user_file = os.path.join(os.path.expanduser("~"),"env.conf")
            config_os_file = os.path.join(konix_config, "env_"+os.sys.platform+".conf")
            config_os_user_file = os.path.join(os.path.expanduser("~"),"env_"+os.sys.platform+".conf")
            if backup_file:
                logging.debug("Beginning with the backup env "+backup_file+" that will override the default env")
                config = getConfigFromEnvFile(backup_file,config)
            logging.debug("Parsing "+config_file)
            config = getConfigFromEnvFile(config_file,config)
            logging.debug("Parsing "+config_user_file)
            config = getConfigFromEnvFile(config_user_file,config)
            logging.debug("Parsing "+config_os_file)
            config = getConfigFromEnvFile(config_os_file,config)
            logging.debug("Parsing "+config_os_user_file)
            config = getConfigFromEnvFile(config_os_user_file,config)
            config["KONIX_ENV_DONE"] = "1"
        else:
            logging.debug("Ignoring the env")
            config = {}
    # Now, I can display the env
    for key in config.keys():
        print "%s=\"%s\"" % (key, config[key])

if __name__ == '__main__':
    main()
