#!/bin/bash

#sort properly
LC_ALL=C

wget -q -O - http://adblockplus.mozdev.org/easylist/easylist.txt | sort |
sed -n '
        1iglobal:
        1i//iframe[@name="google_ads_frame"]

        # delete lines
        /^\[/d # first line
        /^!/d # comments
        /+>/d # weird lines
        /[]]/d # cannot deal with selectors

        h

        # these characters are not nice
        /[^:]/{
                /##/{
                        # global
                        /^##/{
                                g
                                s!^###\(.*\)!\t//*[@id="\1"]!p
                                g
                                s!^##\.\(.*\)!\t//*[@class="\1"]!p
                                d
                        }

                        # not global
                        /^[^#]*##/{
                                g
                                s!^\([^#]*\)###\(.*\)!\n\1:\n\t//*[@id="\2"]!p
                                g
                                s!^\([^#]*\)##\.\(.*\)!\n\1:\n\t//*[@class="\2"]!p
                                d
                        }
                }
        }
' | sed "s/\"/'/g" > $XDG_DATA_HOME/uzbl/adblock
