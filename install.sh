#!/bin/bash

# ************************************************************
# SHELL INSTALL
# ************************************************************
./install_shell.sh
./install_emacs.sh

# ************************************************************
# OLD INSTALL
# ************************************************************

CONFIG="$(pwd)/config"
TUNNING="$(pwd)/tunning"

function remplace(){
    rm -vrf "$1"
    ln -sv "$2" "$1"
}

function install_icon_theme(){
	archive="$1"
	icons_dir="$HOME/.icons"
	echo "install $archive icon theme"
	mkdir "$icons_dir" 2>/dev/null
	tmp_dir="$(mktemp -d)"
	tar -C "$tmp_dir" -xf "$archive"
	theme_name=$(ls --color=never "$tmp_dir")
	rm -rf "$icons_dir/$theme_name"
	mv "$tmp_dir/$theme_name" "$icons_dir/"
	gconftool -s /desktop/gnome/interface/icon_theme -t string "$theme_name"
	rm -rf "$tmp_dir"
}

function install_gtk_theme(){
	archive="$1"
	themes_dir="$HOME/.themes"
	echo "install $archive theme"
	mkdir "$themes_dir" 2>/dev/null
	tmp_dir="$(mktemp -d)"
	tar -C "$tmp_dir" -xf "$archive"
	theme_name=$(ls --color=never "$tmp_dir")
	rm -rf "$themes_dir/$theme_name"
	mv "$tmp_dir/$theme_name" "$themes_dir/"
	gconftool -s /desktop/gnome/interface/gtk_theme -t string "$theme_name"
	rm -rf "$tmp_dir"
}

remplace "$HOME/.gitk"             "$CONFIG/gitk"
remplace "$HOME/.gitconfig"        "$CONFIG/gitconfig"
remplace "$HOME/.fluxbox"          "$CONFIG/fluxbox"
remplace "$HOME/.pyrc.py"          "$CONFIG/pyrc.py"
remplace "$HOME/.conky.d"          "$CONFIG/conky"
remplace "$HOME/.B.blend"          "$CONFIG/B.blend"
remplace "$HOME/.ideskrc"          "$CONFIG/ideskrc"
remplace "$HOME/.idesktop"         "$CONFIG/idesktop"
remplace "$HOME/.Xdefaults"        "$CONFIG/Xdefaults"
remplace "$HOME/Makefile"          "$CONFIG/Makefile"
remplace "$HOME/.gconf/apps/gnome-terminal/profiles/Default/%gconf.xml"        "$CONFIG/config/gnome-terminal-conf.xml"
remplace "$HOME/.xinitrc"          "$CONFIG/xinitrc"
remplace "$HOME/.xsession"         "$HOME/.xinitrc"

remplace "$HOME/bin"               "$(pwd)/bin"
remplace "$HOME/lib"               "$(pwd)/lib"

# ####################################################################################################
# Gnome conf
# ####################################################################################################
install_icon_theme "$TUNNING/Delta_Gnome_Icons.tar.gz"
install_gtk_theme "$TUNNING/Theme.tar.gz"
gconftool --load "$TUNNING/gnome-terminal-profile.xml" /apps/gnome-terminal/profiles/Default
gconftool --load "$TUNNING/gnome-desktop-session.xml" /desktop/gnome/session
gconftool --load "$TUNNING/guake.xml" /apps/guake
gcc config/gnome-run.c -o $HOME/bin/gnome-run -L/usr/X11R6/lib -lX11

chmod +x -Rv "$HOME/bin" "$HOME/.fluxbox/startup" "$HOME/.fluxbox/bin"
