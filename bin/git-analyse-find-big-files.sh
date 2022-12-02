#!/bin/bash

# http://naleid.com/blog/2012/01/17/finding-and-purging-big-files-from-git-history
if ! [ -f bigobjects.txt ]
then
    echo "git-find-big-blobs.sh"
    git-find-big-blobs.sh > bigobjects.txt
fi

if ! [ -f allfileshas.txt ]
then
    echo "git-generate-blob-to-sha-list.sh"
    git-generate-blob-to-sha-list.sh > allfileshas.txt
fi


echo "Analyzing the sha list"
rm -f bigtosmall.txt
for SHA in `cut -f 1 -d\  < bigobjects.txt`
do
    echo $(grep $SHA bigobjects.txt) $(grep $SHA allfileshas.txt) | awk '{print $1,$3,$7}' >> bigtosmall.txt
done;
