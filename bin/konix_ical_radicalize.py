#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sys
import os
import shutil
from uuid import uuid4

import vobject

colors = {
    "calendar_work": "#0000cd"
}


def main(cal):
    dir = os.path.splitext(cal)[0]
    if os.path.exists(dir):
        shutil.rmtree(dir)
    os.makedirs(dir)
    calname = os.path.basename(dir)
    for co in next(vobject.readComponents(
            open(cal).read())).components():
        open(f"{dir}/{co.contents['uid'][0].value}.ics", "w").write(f"""BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//konubinix//Emacs with Org mode//EN
CALSCALE:GREGORIAN
X-WR-CALDESC:
X-WR-CALNAME:OrgMode
X-WR-TIMEZONE:CEST
{co.serialize()}END:VCALENDAR"""
        )
    open(f"{dir}/.Radicale.props", "w").write(f"""{{"C:calendar-timezone": "BEGIN:VCALENDAR\\nBEGIN:VTIMEZONE\\nTZID:Europe/Paris\\nTZURL:http://tzurl.org/zoneinfo/Europe/Paris\\nX-LIC-LOCATION:Europe/Paris\\nBEGIN:DAYLIGHT\\nTZOFFSETFROM:+0100\\nTZOFFSETTO:+0200\\nTZNAME:CEST\\nDTSTART:19810329T020000\\nRRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU\\nEND:DAYLIGHT\\nBEGIN:STANDARD\\nTZOFFSETFROM:+0200\\nTZOFFSETTO:+0100\\nTZNAME:CET\\nDTSTART:19961027T030000\\nRRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU\\nEND:STANDARD\\nBEGIN:STANDARD\\nTZOFFSETFROM:+000921\\nTZOFFSETTO:+000921\\nTZNAME:PMT\\nDTSTART:18910315T010100\\nRDATE:18910315T010100\\nEND:STANDARD\\nBEGIN:STANDARD\\nTZOFFSETFROM:+000921\\nTZOFFSETTO:+0000\\nTZNAME:WET\\nDTSTART:19110311T010100\\nRDATE:19110311T010100\\nEND:STANDARD\\nBEGIN:DAYLIGHT\\nTZOFFSETFROM:+0000\\nTZOFFSETTO:+0100\\nTZNAME:WEST\\nDTSTART:19160615T000000\\nRDATE:19160615T000000\\nRDATE:19170325T000000\\nRDATE:19180310T000000\\nRDATE:19190302T000000\\nRDATE:19200215T000000\\nRDATE:19210315T000000\\nRDATE:19220326T000000\\nRDATE:19230527T000000\\nRDATE:19240330T000000\\nRDATE:19250405T000000\\nRDATE:19260418T000000\\nRDATE:19270410T000000\\nRDATE:19280415T000000\\nRDATE:19290421T000000\\nRDATE:19300413T000000\\nRDATE:19310419T000000\\nRDATE:19320403T000000\\nRDATE:19330326T000000\\nRDATE:19340408T000000\\nRDATE:19350331T000000\\nRDATE:19360419T000000\\nRDATE:19370404T000000\\nRDATE:19380327T000000\\nRDATE:19390416T000000\\nRDATE:19400225T030000\\nEND:DAYLIGHT\\nBEGIN:STANDARD\\nTZOFFSETFROM:+0100\\nTZOFFSETTO:+0000\\nTZNAME:WET\\nDTSTART:19161002T010000\\nRDATE:19161002T010000\\nRDATE:19171008T010000\\nRDATE:19181007T010000\\nRDATE:19191006T010000\\nRDATE:19201024T010000\\nRDATE:19211026T010000\\nRDATE:19221008T010000\\nRDATE:19231007T010000\\nRDATE:19241005T010000\\nRDATE:19251004T010000\\nRDATE:19261003T010000\\nRDATE:19271002T010000\\nRDATE:19281007T010000\\nRDATE:19291006T010000\\nRDATE:19301005T010000\\nRDATE:19311004T010000\\nRDATE:19321002T010000\\nRDATE:19331008T010000\\nRDATE:19341007T010000\\nRDATE:19351006T010000\\nRDATE:19361004T010000\\nRDATE:19371003T010000\\nRDATE:19381002T010000\\nRDATE:19391119T010000\\nEND:STANDARD\\nBEGIN:DAYLIGHT\\nTZOFFSETFROM:+0100\\nTZOFFSETTO:+0200\\nTZNAME:CEST\\nDTSTART:19400614T230000\\nRDATE:19400614T230000\\nRDATE:19430329T020000\\nRDATE:19440403T020000\\nRDATE:19760328T010000\\nRDATE:19770403T020000\\nRDATE:19780402T020000\\nRDATE:19790401T020000\\nRDATE:19800406T020000\\nEND:DAYLIGHT\\nBEGIN:STANDARD\\nTZOFFSETFROM:+0200\\nTZOFFSETTO:+0100\\nTZNAME:CET\\nDTSTART:19421102T030000\\nRDATE:19421102T030000\\nRDATE:19431004T030000\\nRDATE:19450916T030000\\nRDATE:19760926T010000\\nRDATE:19770925T030000\\nRDATE:19781001T030000\\nRDATE:19790930T030000\\nRDATE:19800928T030000\\nRDATE:19810927T030000\\nRDATE:19820926T030000\\nRDATE:19830925T030000\\nRDATE:19840930T030000\\nRDATE:19850929T030000\\nRDATE:19860928T030000\\nRDATE:19870927T030000\\nRDATE:19880925T030000\\nRDATE:19890924T030000\\nRDATE:19900930T030000\\nRDATE:19910929T030000\\nRDATE:19920927T030000\\nRDATE:19930926T030000\\nRDATE:19940925T030000\\nRDATE:19950924T030000\\nEND:STANDARD\\nBEGIN:DAYLIGHT\\nTZOFFSETFROM:+0200\\nTZOFFSETTO:+0200\\nTZNAME:WEMT\\nDTSTART:19440825T000000\\nRDATE:19440825T000000\\nEND:DAYLIGHT\\nBEGIN:DAYLIGHT\\nTZOFFSETFROM:+0200\\nTZOFFSETTO:+0100\\nTZNAME:WEST\\nDTSTART:19441008T020000\\nRDATE:19441008T020000\\nEND:DAYLIGHT\\nBEGIN:DAYLIGHT\\nTZOFFSETFROM:+0100\\nTZOFFSETTO:+0200\\nTZNAME:WEMT\\nDTSTART:19450402T020000\\nRDATE:19450402T020000\\nEND:DAYLIGHT\\nBEGIN:STANDARD\\nTZOFFSETFROM:+0100\\nTZOFFSETTO:+0100\\nTZNAME:CET\\nDTSTART:19770101T000000\\nRDATE:19770101T000000\\nEND:STANDARD\\nEND:VTIMEZONE\\nEND:VCALENDAR\\n", "D:displayname": "{calname}", "ICAL:calendar-color": "{colors.get(calname, "#8BC34AFF")}", "tag": "VCALENDAR"}}""")


if __name__ == "__main__":
    main(sys.argv[1])