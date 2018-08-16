#!/bin/bash

VERSION=4.2
mkdir -p "${HOME}/local"
wget -O "${HOME}/local/LanguageTool-${VERSION}.zip" "https://www.languagetool.org/download/LanguageTool-${VERSION}.zip"
pushd "${HOME}/local"
unzip "LanguageTool-${VERSION}.zip"
rm -rf "languagetool-commandline.jar"
ln -sr "LanguageTool-${VERSION}/languagetool-commandline.jar"
popd
