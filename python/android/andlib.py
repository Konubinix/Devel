import android

def items2assoc(items):
    return [(str(item), item,) for item in items]

class Chooser(object):
    def __init__(self, droid):
        self.droid = droid
        self.items = None

    def choose_show(self, msg, items, positive=None, negative=None):
        self.droid.dialogCreateAlert(
            msg
            )
        if positive is not None:
            self.droid.dialogSetPositiveButtonText(positive)
        if negative is not None:
            self.droid.dialogSetNegativeButtonText(negative)
        self.items = items
        keys = []
        for i in items:
            if type(i)==tuple:
                keys.append(i[0])
            else:
                keys.append(i)
        self.droid.dialogSetItems(
            keys
            )
        self.droid.dialogShow()

    def choose_get_res(self):
        items = self.items
        res = self.droid.dialogGetResponse().result
        button = response_button(res)
        if button is not None:
            return button
        item_index = res.get("item", None)
        if item_index is None:
            return None
        item = items[item_index]
        self.droid.dialogDismiss()
        if type(item)==tuple:
            return item[1]
        else:
            return item

    def choose(self, msg, items, positive=None, negative=None):
        self.choose_show(msg, items, positive, negative)
        return self.choose_get_res()

def confirm(droid, title, msg):
    droid.dialogCreateAlert(title, msg)
    droid.dialogSetPositiveButtonText("Yes")
    droid.dialogSetNegativeButtonText("No")
    droid.dialogShow()
    response = droid.dialogGetResponse().result
    droid.dialogDismiss()
    return response_button(response)

def response_button(response):
    if response.has_key("which"):
        result = response["which"]
        if result == "positive":
            return True
        else:
            return False
    return None

def show_progress(droid, title, msg, progress, max):
    droid.dialogDismiss()
    droid.dialogCreateHorizontalProgress(
        title,
        msg,
        max
            )
    droid.dialogShow()
    droid.dialogSetCurrentProgress(progress)

def display(droid, title, msg):
    droid.dialogCreateAlert(title, msg)
    droid.dialogShow()
    droid.dialogGetResponse()

def input(droid, title, message=None, defaulttext=None, inputtype=None):
    droid.dialogCreateInput(title, message, defaulttext, inputtype)
    droid.dialogSetPositiveButtonText("Ok")
    droid.dialogSetNegativeButtonText("Cancel")
    droid.dialogShow()
    return droid.dialogGetResponse().result
