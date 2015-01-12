#!/bin/bash

cd "$(git -c core.bare=false rev-parse --show-toplevel)"
echo Total size
du -sh .
echo Annex size
du -sh .git/annex/
echo Working copy
du -sh --exclude .git .
echo Git object size
du -sh .git/objects
echo Git object tmp size
du -sch .git/objects/pack/tmp*
echo Size in git annexed files here
git annex info --fast --in here .
