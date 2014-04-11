#!/usr/bin/env python
# -*- coding:utf-8 -*-

import os
import subprocess
import shlex
import sys
import logging
logging.basicConfig()
logger = logging.getLogger("unseen")
logger.setLevel(logging.INFO)
logger.debug(" ".join(sys.argv))

p = subprocess.Popen(shlex.split("git rev-parse --git-dir"),
                stdout=subprocess.PIPE,
                stderr=open("/dev/null", "w"),
              )
p.wait()

in_git_annex = False
if p.returncode == 0:
  git_dir = p.stdout.read()[:-1]
  in_git_annex = os.path.isdir(os.path.join(git_dir, "annex"))

if not ( in_git_annex and
         os.environ.get("KONIX_GIT_ANNEX_DIRED_METADATA", None) ):
  subprocess.call(["ls"] + sys.argv[1:])
else:
  filter = os.environ["KONIX_GIT_ANNEX_DIRED_METADATA"]
  logger.info("git annex filter: %s" % filter)
  # look for the -- in the arguments
  index = 0
  found = False
  for arg in sys.argv:
    if arg == "--":
      found = True
      break
    index = index + 1
  if found:
    ls_args = sys.argv[1:index]
    input_directories = sys.argv[index+1:]
  else:
    logger.info("-- not found, default to .")
    ls_args = sys.argv[1:]
    input_directories = [".",]
  assert len(input_directories) == 1, "Only support one input directory for the time being"
  logger.debug("Args: " + " ".join(ls_args))
  logger.debug("Input: " + " ".join(input_directories))
  directories = []
  files = []
  for directory in input_directories:
      files = files + [
          os.path.join(directory, f)
          for f in os.listdir(directory)]
  directories = [d for d in files if os.path.isdir(d)]
  files = list(set(files).difference(set(directories)))
  ignored_command = []
  if len(files) != 0:
    os.chdir(input_directories[0])
    files = [os.path.relpath(f) for f in files]
    command = ["git", "annex", "find", "--not", "-(",] + \
              shlex.split(filter) + \
              ["-)", '--'] + files
    logger.debug(" ".join(command))
    p = subprocess.Popen(command,
          stdout=subprocess.PIPE)
    p.wait()
    ignored_files = [f[:-1] for f in p.stdout.readlines()]
    for ignored_file in ignored_files:
        ignored_command.append("-I")
        ignored_command.append(ignored_file)
  ls_command = ["ls",] + ls_args + ignored_command + ["--",] + input_directories
  logger.debug(" ".join(ls_command))
  subprocess.call(ls_command)
