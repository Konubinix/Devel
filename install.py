#!/usr/bin/env python
# -*- coding:utf-8 -*-

import os
import sys
import re
import shutil
import tarfile
import subprocess
import logging
logging.basicConfig(level=logging.DEBUG)
import tempfile
from string import Template
from tempfile import NamedTemporaryFile

# ####################################################################################################
# Fill the environ variables
# ####################################################################################################
from install_lib import *
environ = get_environ()
# ####################################################################################################
# Install ~/.env.conf default env file. It is the only absolute prerequisite for
# everything else
# ####################################################################################################
DEFAULT_ENV_FILE_NAME = os.path.join(os.path.expanduser("~"),".env_"+sys.platform+".conf")
DEFAULT_ENV_FILE_CONTENT = "[replace]\n"
for key in environ:
        DEFAULT_ENV_FILE_CONTENT += key+"="+environ[key]+"\n"
replace_file_content(DEFAULT_ENV_FILE_NAME, DEFAULT_ENV_FILE_CONTENT)

# ####################################################################################################
# Put the default ~/bin folder and put the env getters
# ####################################################################################################
substitute(os.path.join(environ["KONIX_PWD"],"init_bin"), os.path.join(environ["HOME"],"init_bin"))

# ####################################################################################################
# Install shell and emacs
# ####################################################################################################
from install_emacs import install_emacs
from install_shell import install_shell
from install_git import install_git
install_shell()
install_emacs()
install_git()

substitute(os.path.join(environ["CONFIG_DIR"], "Makefile"),   os.path.join(environ["HOME"], "Makefile"))
substitute(os.path.join(environ["CONFIG_DIR"], "gitk"),       os.path.join(environ["HOME"], ".gitk"))
substitute(os.path.join(environ["CONFIG_DIR"], "inputrc"),    os.path.join(environ["HOME"], ".inputrc"))
substitute(os.path.join(environ["CONFIG_DIR"], "B.blend"),    os.path.join(environ["HOME"], ".B.blend"))
substitute(os.path.join(environ["CONFIG_DIR"], "screenrc"),    os.path.join(environ["HOME"], ".screenrc"))
substitute(os.path.join(environ["CONFIG_DIR"], "vimrc"),      os.path.join(environ["HOME"], ".vimrc"))

if is_on_linux():
        substitute(os.path.join(environ["CONFIG_DIR"], "fluxbox"),   os.path.join(environ["HOME"], ".fluxbox"))
        substitute(os.path.join(environ["DEVEL_DIR"],  "bin"),       os.path.join(environ["HOME"], ".fluxbox/devel_bin"))
        substitute(os.path.join(environ["CONFIG_DIR"], "conky"),     os.path.join(environ["HOME"], ".conky.d"))
        substitute(os.path.join(environ["CONFIG_DIR"], "ideskrc"),   os.path.join(environ["HOME"], ".ideskrc"))
        substitute(os.path.join(environ["CONFIG_DIR"], "idesktop"),  os.path.join(environ["HOME"], ".idesktop"))
        substitute(os.path.join(environ["CONFIG_DIR"], "Xdefaults"), os.path.join(environ["HOME"], ".Xdefaults"))
        substitute(os.path.join(environ["CONFIG_DIR"], "xinitrc"),   os.path.join(environ["HOME"], ".xinitrc"))
        substitute(os.path.join(environ["HOME"],       ".xinitrc"),  os.path.join(environ["HOME"], ".xsession"))
        substitute(os.path.join(environ["CONFIG_DIR"], "gtkrc-2.0"), os.path.join(environ["HOME"], ".gtkrc-2.0"))

        # ####################################################################################################
        # Gnome conf
        # ####################################################################################################
        def install_icon_theme(archive):
                icons_dir=os.path.join(environ["HOME"],".icons")
                logging.info("install %s icon theme" % archive)
                if not os.path.isdir(icons_dir):
                        os.makedirs(icons_dir)
                tempdir = tempfile.mkdtemp()
                archive_tar = tarfile.open(name=archive)
                archive_tar.extractall(path=tempdir)
                theme_name = os.listdir(tempdir)
                if len(theme_name) > 1:
                        logging.warning("Bad icon archive "+archive)
                        return False
                else:
                        theme_name = theme_name[0]
                new_icons_dir = os.path.join(icons_dir, theme_name)
                if confirm(prompt="Remove old icons dir "+new_icons_dir):
                        shutil.rmtree(new_icons_dir)
                else:
                        return False
                shutil.move(os.path.join(tempdir, theme_name), new_icons_dir)
                subprocess.call(["gconftool", "-s", "/desktop/gnome/interface/icon_theme", "-t", "string", theme_name])
                shutil.rmtree(tempdir)
        def install_gtk_theme(archive):
                themes_dir = os.path.join(environ["HOME"], ".themes")
                logging.info("install %s theme" % archive)
                if not os.path.isdir(themes_dir):
                        os.makedirs(themes_dir)
                tempdir = tempfile.mkdtemp()
                archive_tar = tarfile.open(name=archive)
                archive_tar.extractall(path=tempdir)
                theme_name = os.listdir(tempdir)
                if len(theme_name) > 1:
                        logging.warning("Bad gtk archive "+archive)
                        return False
                else:
                        theme_name = theme_name[0]
                new_theme_dir = os.path.join(themes_dir, theme_name)

                if confirm(prompt="Remove old theme dir "+new_theme_dir):
                        shutil.rmtree(new_theme_dir)
                else:
                        return False
                shutil.move(os.path.join(tempdir, theme_name), new_theme_dir)
                subprocess.call(["gconftool", "-s", "/desktop/gnome/interface/gtk_theme", "-t", "string", theme_name])
                shutil.rmtree(tempdir)

        install_icon_theme (os.path.join(environ["TUNNING_DIR"], "Delta_Gnome_Icons.tar.gz"))
        install_gtk_theme  (os.path.join(environ["TUNNING_DIR"], "Theme.tar.gz"))
        subprocess.call(["gconftool", "--load", os.path.join(environ["TUNNING_DIR"], "gnome-terminal-profile.xml"), "/apps/gnome-terminal/profiles/Default"])
        subprocess.call(["gconftool", "--load", os.path.join(environ["TUNNING_DIR"], "gnome-desktop-session.xml"), "/desktop/gnome/session"])
        subprocess.call(["gconftool", "--load", os.path.join(environ["TUNNING_DIR"], "guake.xml"), "/apps/guake"])
        subprocess.call(["gconftool", "--load", os.path.join(environ["TUNNING_DIR"], "notification-daemon.xml"), "/apps/notification-daemon"])
        # Set emacs style bindings to firefox
        subprocess.call(["gconftool-2", "--set", "/desktop/gnome/interface/gtk_key_theme Emacs", "--type", "string"])
        subprocess.call(["gcc", "config/gnome-run.c", "-o", os.path.join(environ["HOME"],".fluxbox/bin/gnome-run"), "-L/usr/X11R6/lib", "-lX11"])
        # install the emacs_tray_daemon
        # export the code from the template
        TRAY_PROGRAM_TPL_FILE = open(os.path.join(environ["CONFIG_DIR"],"emacs_tray_daemon.c.tpl"), "r")
        TRAY_PROGRAM_FILE = NamedTemporaryFile(delete=False, suffix=".c")
        TRAY_PROGRAM_TPL = Template(TRAY_PROGRAM_TPL_FILE.read())
        TRAY_PROGRAM_FILE.write(TRAY_PROGRAM_TPL.substitute(environ))
        TRAY_PROGRAM_FILE.close()
        subprocess.call(["sh","-c", "gcc '%s' `pkg-config --cflags --libs gtk+-2.0` -o %s/emacs_tray_daemon"
                         %(TRAY_PROGRAM_FILE.name,
                           os.path.join(os.environ["HOME"],"bin")
                           )
                         ])
        os.remove(TRAY_PROGRAM_FILE.name)
        subprocess.call(["chmod", "+x", "-Rv",
                         os.path.join(environ["HOME"], "bin"),
                         os.path.join(environ["HOME"], ".fluxbox/startup"),
                         os.path.join(environ["HOME"], ".fluxbox/bin")
                         ])
