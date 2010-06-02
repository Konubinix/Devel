#!/bin/bash

CONFIG=$(pwd)/config

function remplace(){
    rm -vrf $1
    ln -sv $2 $1
}

remplace $HOME/.gitk             $CONFIG/gitk
remplace $HOME/.gitconfig        $CONFIG/gitconfig
remplace $HOME/.emacs            $CONFIG/emacs
remplace $HOME/.emacs-custom.el  $CONFIG/emacs-custom.el
remplace $HOME/.bash_aliases     $CONFIG/bash_aliases
remplace $HOME/.bashrc           $CONFIG/bashrc
remplace $HOME/.fluxbox          $CONFIG/fluxbox
remplace $HOME/.pyrc.py          $CONFIG/pyrc.py
remplace $HOME/.elfiles          $CONFIG/elfiles
remplace $HOME/.conky.d          $CONFIG/conky
remplace $HOME/.B.blend          $CONFIG/B.blend
remplace $HOME/.ideskrc          $CONFIG/ideskrc
remplace $HOME/.idesktop         $CONFIG/idesktop
remplace $HOME/.zshrc            $CONFIG/zshrc
remplace $HOME/.zsh_aliases      $CONFIG/zsh_aliases
remplace $HOME/.shrc             $CONFIG/shrc
remplace $HOME/.Xdefaults        $CONFIG/Xdefaults
remplace $HOME/Makefile          $CONFIG/Makefile

remplace $HOME/bin               $(pwd)/bin
remplace $HOME/lib               $(pwd)/lib

chmod +x -Rv $HOME/bin $HOME/.fluxbox/startup $HOME/.fluxbox/bin
