#!/bin/bash


mkdir -p "${HOME}/local"
wget -O "${HOME}/local/LanguageTool-3.9.zip" https://www.languagetool.org/download/LanguageTool-3.9.zip
pushd "${HOME}/local"
unzip LanguageTool-3.9.zip
popd
