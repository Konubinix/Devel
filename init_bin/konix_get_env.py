#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sys
import os
import configparser
import string
import re
import logging
import socket
import random
import glob

logging.basicConfig(level=logging.DEBUG)
default_config_file = os.path.join(os.path.expanduser("~"),".env_"+os.sys.platform+".conf")
DEFAULT_CONFIG={
    'KONIX_PWD' : os.getcwd(),
    'SPACE' : " ",
    'DOLLAR' : '$',
    }

ENV_BACKUP_FILE_TEMPLATE=os.path.join(os.path.expanduser("~"),"env_backups",".env_backup_%s_"+os.sys.platform+".conf")

# by default, get the values from os.environ
DEFAULT_ENVIRON = os.environ

def mergeItemsOfSection(items):
    # this method assumes that the items are unique
    new_items = {}
    items_keys = []
    for key,value in items:
        if new_items.get(key) == None:
            new_items[key] = value
        else:
            new_items[key] = "".join((new_items[key],value,))
        items_keys.append(key)
    return (new_items, items_keys,)

def parse_value(key, value, previous_config, new_config):
    # the value may be evaled
    if value.startswith("eval:"):
        value = eval(value.replace("eval:", ""))
    # first step is to make sure to correctly unquote the values
    value = re.sub('^"(.*)"$', r"'\1'", value)
    value = re.sub("^'(.*)'$", r"\1", value)

    env_value = DEFAULT_ENVIRON.get(key)
    if env_value == None:
        env_value = ""

    value = value.replace('\n','')
    substitute_config = {}
    substitute_config.update(DEFAULT_CONFIG)
    substitute_config.update(DEFAULT_ENVIRON)
    substitute_config.update(previous_config)
    substitute_config.update(new_config)

    value_template = string.Template(value)
    value = value_template.safe_substitute(substitute_config)
    return value, env_value

def getConfigFromEnvFile(envfile, previous_config):
    if not os.path.exists(envfile):
        print(f"Config file {envfile} does not exist", file=sys.stderr)
        return previous_config

    config = configparser.ConfigParser(strict=False)
    config.optionxform = str    # keys not converted into lower case
    config.read(envfile)
    new_config = previous_config
    for section in config.sections():
        if section == "include":
            items = config.items(section, raw=True)
            for item in items:
                condition, file_to_include=item
                file_to_include, _ = parse_value(
                  condition,
                  file_to_include,
                  previous_config,
                  new_config,
                )
                logging.debug("## Attempting to include env from %s (%s)" %
                              (file_to_include, condition,))
                if condition == "assert_exists":
                  assert os.path.exists(file_to_include)
                elif not os.path.exists(file_to_include):
                  logging.critical("File %s not found" % file_to_include)
                # update the existing config with the one included
                new_config = getConfigFromEnvFile(file_to_include, new_config)
                logging.debug("## Attempted to include env from %s (%s)" %
                              (file_to_include, condition,))
            continue

        assert(section in ("prefix","replace","suffix",))
        (items, items_keys,) = mergeItemsOfSection(config.items(section, raw=True))
        for key in items_keys:
            value = items[key]
            value, env_value = parse_value(key, value, previous_config, new_config)

            #env_value = env_value.replace("\\","\\\\")
            #value = value.replace('\\','\\\\')

            # the previous value is either set by a previous step in
            # the loop
            if new_config.get(key) != None:
                previous_value = new_config[key]
            # or got from the env_value
            else:
                previous_value = env_value

            logging.debug("analysing ["+key+"] = "+value+"\n\t(previous was "+previous_value+")")
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
    # create the dir
    if not os.path.exists(os.path.dirname(env_backup_file_name)):
        os.makedirs(os.path.dirname(env_backup_file_name))
    if os.path.exists(env_backup_file_name):
        os.remove(env_backup_file_name)
    f = open(env_backup_file_name, "w")
    f.write("[replace]\n")
    for key, value in list(os.environ.items()):
        f.write(key+"='"+value.replace("\n", "\n\t")+"'\n")
    f.close()

def createBackupFileOrUseIt(stamp):
    backup_file = None
    if not stamp:
        stamp = str(random.randint(0,300000))
        logging.debug("Creating new backup env file with stamp "+stamp)
    if not os.path.exists(getBackupEnvFileName(stamp)):
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
        logging.debug("## Parsing " + GIVEN_CONFIG_FILE)
        config = getConfigFromEnvFile(GIVEN_CONFIG_FILE, config)
    else:
        logging.debug("## Parsing "+ default_config_file)
        config = getConfigFromEnvFile(default_config_file,config) # get the default config to know the platform

        # changing of platform must force a new env to be loaded. Then the env loading is done if
        # - the platforms are differents
        # or
        # - env_done is not set to 1

        logging.debug("the config is for "+config["KONIX_PLATFORM"]+", the environ says "+os.environ.get("KONIX_PLATFORM", "nothing"))
        if config["KONIX_PLATFORM"] != os.environ.get("KONIX_PLATFORM") or os.environ.get("KONIX_ENV_IGNORE", None) == None:
            # ####################################################################################################
            # Hardcoded ones
            # ####################################################################################################
            config["HOSTNAME"] = config.get("HOSTNAME", socket.gethostname())
            (stamp, backup_file) = createBackupFileOrUseIt(os.environ.get("KONIX_ENV_STAMP", None))
            config["KONIX_ENV_STAMP"] = stamp
            logging.debug("## Parsing the env")
            devel_dir = config["KONIX_DEVEL_DIR"]
            konix_config = os.path.join(devel_dir,"config")
            config_file = os.path.join(konix_config, "env.conf")
            config_user_file = os.path.join(os.path.expanduser("~"),"env.conf")
            config_os_file = os.path.join(konix_config, "env_"+os.sys.platform+".conf")
            config_os_user_file = os.path.join(os.path.expanduser("~"),"env_"+os.sys.platform+".conf")
            if backup_file:
                logging.debug("## Beginning with the backup env "+backup_file+" that will override the default env")
                config = getConfigFromEnvFile(backup_file,config)
            logging.debug("## Parsing "+config_file)
            config = getConfigFromEnvFile(config_file,config)
            logging.debug("## Parsing "+config_user_file)
            config = getConfigFromEnvFile(config_user_file,config)
            logging.debug("## Parsing "+config_os_file)
            config = getConfigFromEnvFile(config_os_file,config)
            logging.debug("## Parsing "+config_os_user_file)
            config = getConfigFromEnvFile(config_os_user_file,config)
        else:
            logging.debug("Ignoring the env")
            config = {}
    # Now, I can display the env
    for key in list(config.keys()):
        # make sure the final value will be correctly quoted
        print("%s='%s'" % (key, config[key]))

if __name__ == '__main__':
    main()
