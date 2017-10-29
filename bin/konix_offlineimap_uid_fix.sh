#!/bin/bash -eux

# taken from http://www.offlineimap.org/doc/FAQ.html#what-is-the-uid-validity-problem-for-folder

Name="$1"
AccountName="$(python -c "print('${Name}'.lower())")"

mkdir -p ~/archives
mv ~/.offlineimap/Account-${AccountName} ~/archives/Account-${AccountName}
mv ~/.offlineimap/Repository-Remote${Name} ~/archives/Repository-RemoteRepository${Name}
mv ~/.offlineimap/Repository-Local${Name} ~/archives/Repository-LocalRepository${Name}
