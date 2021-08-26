#! /usr/bin/env python3

from konix_android import droid
import ril
import andlib
import os
import sys

def chooseitem(items):
    c = andlib.Chooser(droid)
    return c.choose(
        "Choose item",
        andlib.items2assoc(items)
        )

def toggle_read(item):
    item.read = not item.read
    compute_items()

def do_all(items, do, rescb, title, msg=""):
    res = []
    nmax = len(items)
    n = 0
    droid.dialogCreateHorizontalProgress(
            title,
            msg,
            nmax
    )
    droid.dialogShow()
    for item in items:
        r = do(item)
        n += 1
        res.append(rescb(item, r))
        droid.dialogSetCurrentProgress(n)
    droid.dialogDismiss()
    c = andlib.Chooser(droid)
    c.choose("Results", res)
    return res

def dl_all(items, mode):
    def rescb(item, res):
        return "%s\n%s" % (
                item,
                ril.download_rc_to_msg(res)
            )

    do_all(
            items,
            lambda  x: x.download(mode),
            rescb,
            "DLing"
            )

def clean_all(items):
    def rescb(item, res):
        return "%s\n%s" % (
                item,
                "OK"
            )

    do_all(
            items,
            lambda  x: x.clean(),
            rescb,
            "Cleaning"
            )

def dl_item(item, mode=ril.DOWNLOAD_NORMAL_MODE):
        droid.dialogCreateSpinnerProgress(
            "Wait",
            "Wait or the page to be DLed"
            )
        droid.dialogShow()
        print "DLing"
        r = item.download(mode)
        droid.dialogDismiss()
        if r != 0:
            droid.makeToast(ril.download_rc_to_msg(r))

def show_wget_log(item):
    c = andlib.Chooser(droid)
    c.choose(
            "Wget log",
            item.wget_log.splitlines()
            )

class Config(object):
    def __init__(self, activated):
        self.activated = activated

    def toggle(self):
        self.activated = not self.activated

class Condition(Config):
    def __init__(self, activated, functor):
        self.activated = activated
        self.functor = functor

    def apply(self, item):
        return self.functor(item)

configs = {
        "recompute":Config(True),
        "auto_select":Config(False),
        "auto_open":Config(False),
        "prior_back":Config(False),
        }

conditions = {
        "dled":Condition(True, lambda x:x.dled),
        "unread":Condition(True, lambda x:not x.read),
        "imp":Condition(False, lambda x:x.priority == "A"),
        "prior impl":Condition(False, lambda x:not x.priority_explicit),
        }

def toggle_condition(condition):
    def toggling_func(item):
        conditions[condition].activated = not conditions[condition].activated
    return toggling_func

def toggle_config(config):
    def toggling_func(item):
        configs[config].activated = not configs[config].activated
    return toggling_func

def get_priority_setter(priority):
    def set_priority(item):
        global items
        item.priority = priority
        items.sort(key=lambda x:x.priority)
        if configs["prior_back"].activated:
            return False
    return set_priority

def compute_items():
    global items
    droid.dialogCreateSpinnerProgress("Please wait", "Computing ril items")
    droid.dialogShow()
    items = ril.rilitems_priority_stamp_sorted()
    droid.dialogDismiss()

def rm_item(item):
   global items
   if andlib.confirm(
                    droid,
                    "Really delete ?",
                    "Do you really want to delete %s" % item):
       item.remove()
       items = [i for i in items if i.hash != item.hash]

def rm_item_and_back(item):
    rm_item(item)
    return False

def menu(chooser):
        choices = (
                ("DL all", lambda x: dl_all(items, ril.DOWNLOAD_NORMAL_MODE)),
                ("DL all light", lambda x: dl_all(items, ril.DOWNLOAD_LIGHT_MODE
                    )),
                ("DL all mirror", lambda x: dl_all(items, ril.DOWNLOAD_MIRROR_MODE)),
                ("Clean all", lambda x: clean_all(items)),
                ("Recompute", lambda x: compute_items()),
                )
        for condition in conditions:
            toggling_func = toggle_condition(condition)
            choices += (
                    (
                    "toggle %s (%s)" % (condition, conditions[condition].activated),
                    toggling_func,
                        ),
                    )
        for config in configs:
            toggling_func = toggle_config(config)
            choices += (
                    (
                    "toggle %s (%s)" % (config, configs[config].activated),
                    toggling_func,
                        ),
                    )
        return chooser.choose(
            "Choose item",
            choices,
            "Back"
            )

def step():
    global items
    for condition in [condition
            for condition in conditions
            if conditions[condition].activated]:
        items = [item
                for item in items
                if conditions[condition].apply(item)]
    if not items:
        droid.makeToast("No more items to see")
        return False
    droid.dialogDismiss()
    chooser = andlib.Chooser(droid)
    conditions_string = ", ".join(
            [condition for condition in conditions
                if conditions[condition].activated
                ]
            )
    if conditions_string:
        conditions_string = " (%s)" % conditions_string
    if configs.get("auto_select").activated:
       item = items[0]
    else:
       item = chooser.choose(
            "Choose item %s%s" % (
                len(items),
                conditions_string,
                ),
            andlib.items2assoc(items),
            "Menu",
            "Quit"
            )
    if type(item) == bool and item == True:
        # menu
        choice = menu(chooser)
        if not ( type(choice) == bool and choice == True ):
            choice(item)
    elif type(item) == bool and item == False:
        droid.makeToast("Bye")
        sys.exit(0)
    elif item is not None:
        # show the menu about the item
        choice = None
        first_time = True
        while choice != False:
            choices = (
                "open",
                "open local web",
                "open origin",
                "to org",
                ("A", get_priority_setter('A')),
                ("B", get_priority_setter('B')),
                ("C", get_priority_setter('C')),
                ("describe", lambda x: chooser.choose(
                    str(x),
                    (
                        str(x),
                        "url: %s" % x.url
                        ),
                    )
                    ),
                ("toggle read", lambda x: toggle_read(x)),
                ("dl", lambda x: dl_item(x)),
                ("dl light", lambda x: dl_item(x, ril.DOWNLOAD_LIGHT_MODE)),
                ("dl mirror", lambda x: dl_item(x, ril.DOWNLOAD_MIRROR_MODE)),
                ("Wget log", show_wget_log),
                ("clean", lambda x: x.clean()),
                ("rm", rm_item),
                ("rm_and_back", rm_item_and_back),
                "back",
            )
            if choice == None and first_time and configs["auto_open"].activated:
                choice = "open"
            else:
                choice = chooser.choose(str(item), choices,
                    "Menu",
                    "Quit"
                    )
            first_time = False
            if type(choice) == bool and choice == True:
                # menu
                choice = menu(chooser)
                if not ( type(choice) == bool and choice == True ):
                    choice(item)
            elif type(choice) == bool and choice == False:
                droid.makeToast("Bye")
                sys.exit(0)
            elif type(choice) == str and choice == "open":
                chooser.choose_show("Opening url, click on ok when done (%s left)"% len(items), (item.title_html_unescaped,), "OK", "Rm and back")
                print "Opening %s" % str(item)
                item.open()
                choice2 = chooser.choose_get_res()
                if choice2 == False:
                    choice = rm_item_and_back(item)
            elif type(choice) == str and choice == "open local web":
                chooser.choose_show("Opening url, click on ok when done (%s left)"% len(items), (item.title_html_unescaped,), "OK", "Rm and back")
                print "Opening %s" % str(item)
                item.open_local_web()
                choice2 = chooser.choose_get_res()
                if choice2 == False:
                    choice = rm_item_and_back(item)
            elif choice is None:
                choice = False
            elif type(choice) == str and choice == "open origin":
                chooser.choose_show("Click on ok when done", (item.url,), "OK")
                os.system("konix_android_browser_open.sh '%s'" % item.url)
                chooser.choose_get_res()
            elif type(choice) == str and choice == "to org":
                chooser.choose_show("Click on ok when done", (item.url,), "OK")
                os.system("echo '%s'|konix_gtd_org.sh" % item.url)
                chooser.choose_get_res()
            elif type(choice) == str and choice == "back":
                choice = False
            elif type(choice) != bool:
                choice = choice(item)
            else:
                assert False, "not handled choice %s" % choice
    return True

def main():
    compute_items()
    while step():
        if configs['recompute'].activated:
            compute_items()
