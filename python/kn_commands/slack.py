#!/usr/bin/env python
# -*- coding:utf-8 -*-

from __future__ import print_function, absolute_import


import os
import re
import datetime
import builtins

import click
import slacker
import parsedatetime

from click_project.decorators import group, table_fields, table_format, \
    argument, param_config, option
from click_project.log import get_logger
from click_project.lib import get_authenticator, TablePrinter,\
    ParameterType, makedirs
from click_project.completion import startswith
from click_project.config import config
from click_project.core import cache_disk

LOGGER = get_logger(__name__)


def get_slack_token():
    values = get_authenticator("slack_token", required=False, askpass=False)
    if values is not None:
        return values[1]
    else:
        return None


class Message():
    def __init__(self, data):
        if "user" not in data:
            if "bot_id" in data:
                data["user"] = data["bot_id"]
                data["username"] = "BOT"
            elif "file" in data:
                data["username"] = data["file"]["name"]
        if "username" not in data:
            data["username"] = config.slack.users[data["user"]]["name"]
        data["orig_text"] = data["text"]
        data["text"] = data["text"].replace("{", "{{").replace("}", "}}")
        data["text"] = re.sub("<@([^>|]+)([|][^>]+)?>", r"{\1}", data["text"])
        data["date"] = datetime.datetime.fromtimestamp(float(data["ts"]))
        data["text"] = data["text"].format(
            **{
                id: user["name"]
                for id, user in config.slack.users.items()
            }
        )
        data["datetime"] = datetime.datetime.fromtimestamp(
            float(data["ts"])).strftime("%Y-%m-%d %H:%M:%S")
        self.data = data

    def __repr__(self):
        return "<{} - {} - {}>".format(
            self.data["datetime"], self.data["username"], self.data["text"])

    def __lt__(self, o):
        return self.data["date"] < o.data["date"]


class Conversation():
    def __init__(self, endpoint, data):
        self.endpoint = endpoint
        self.data = data
        self.id = data["id"]
        self.data["topic_fmt"] = self.topic_fmt
        self.data["num_members"] = self.num_members
        self.data["created_date"] = self.created_date
        self.data["creator_fmt"] = self.creator_fmt
        self.data["purpose_fmt"] = self.purpose_fmt
        self.data["members_fmt"] = self.members_fmt
        self.data["name_fmt"] = self.name

    @property
    def num_members(self):
        return self.data["num_members"]

    @property
    def created_date(self):
        return datetime.datetime.fromtimestamp(self.data["created"])

    @property
    def creator_fmt(self):
        return config.slack.users[self.data["creator"]]["name"]

    @property
    def members_fmt(self):
        return ", ".join(
            config.slack.users[u]["name"] for u in self.data["members"]
        )

    @property
    def purpose_fmt(self):
        return "'{}' mis par {} le {}".format(
            self.data["purpose"]["value"],
            config.slack.users[self.data["purpose"]["creator"]]["name"],
            datetime.datetime.fromtimestamp(
                self.data["purpose"]["last_set"]).strftime(
                    "%Y-%m-%d %H:%M:%S"),
        ) if self.data["purpose"]["creator"] else ""

    @property
    def topic_fmt(self):
        return "'{}' mis par {} le {}".format(
            self.data["topic"]["value"],
            config.slack.users[self.data["topic"]["creator"]]["name"],
            datetime.datetime.fromtimestamp(
                self.data["topic"]["last_set"]).strftime(
                    "%Y-%m-%d %H:%M:%S"),
        ) if self.data["topic"]["creator"] else ""

    @property
    def name(self):
        return self.data["name"]

    def history(self, oldest=None, latest=None):
        def get_history_unpage(latest, oldest):
            res = self.endpoint.history(
                self.data["id"],
                count=1000,
                latest=latest,
                oldest=oldest,
            ).body
            messages = res["messages"]
            if res["has_more"]:
                messages += get_history_unpage(latest=messages[-1]["ts"],
                                               oldest=oldest)
            return messages
        return builtins.sorted([
            Message(message)
            for message in get_history_unpage(oldest=oldest, latest=latest)])

    def __repr__(self):
        return "<{} - {}>".format(self.data["id"], self.name)


class Group(Conversation):
    @property
    def num_members(self):
        return None


class MP(Conversation):
    @property
    def num_members(self):
        return None


class Channel(Conversation):
    pass


class IM(Conversation):
    @property
    def name(self):
        return config.slack.users[self.data["user"]]["name"]

    @property
    def user(self):
        return config.slack.users[self.data["user"]]

    @property
    def topic_fmt(self):
        return "Discussion with {}".format(
            config.slack.users[self.data["user"]].name)

    @property
    def creator_fmt(self):
        return None

    @property
    def purpose_fmt(self):
        return self.topic_fmt

    @property
    def members_fmt(self):
        return self.user.name

    @property
    def num_members(self):
        return 2


class Conversations():
    cls = Conversation

    def list(self):
        return [
            self.cls(self.endpoint, c)
            for c in self.endpoint.list().body[self.list_index]
        ]

    def get(self, id_or_name):
        return [
            c for c in self.list() if c.id == id_or_name
            or c.name == id_or_name
        ][0]


class Groups(Conversations):
    cls = Group
    list_index = "groups"

    @property
    def endpoint(self):
        return config.slack.client.groups


class IMS(Conversations):
    cls = IM
    list_index = "ims"

    @property
    def endpoint(self):
        return config.slack.client.im


class MPIM(Conversations):
    list_index = "groups"
    cls = MP

    @property
    def endpoint(self):
        return config.slack.client.mpim


class Channels(Conversations):
    list_index = "channels"
    cls = Channel

    @property
    def endpoint(self):
        return config.slack.client.channels


class User():
    def __init__(self, data):
        self.data = data
        self.id = data["id"]
        self.name = data["name"]

    def __getitem__(self, item):
        return self.data[item]

    def __repr__(self):
        return "<{}>".format(self.data["name"])


class SlackConfig():
    def __init__(self):
        self._client = None

    @property
    def client(self):
        if self._client is None:
            self._client = slacker.Slacker(self.token)
        return self._client

    @property
    def users(self):
        @cache_disk(expire=36000)
        def _users():
            return {
                user["id"]: User(user)
                for user in self.client.users.list().body["members"]
            }
        return _users()

    def get_user(self, name):
        return [u for u in self.users.values() if u.data["name"] == name][0]

    def get_conversation(self, id_or_name):
        return [
            c for c in self.conversations.values()
            if c.id == id_or_name or c.name == id_or_name
        ][0]

    @property
    def groups(self):
        return [
            c for c in self.conversations.values()
            if isinstance(c, Group)
        ]

    @property
    def channels(self):
        return [
            c for c in self.conversations.values()
            if isinstance(c, Channel)
        ]

    @property
    def ims(self):
        return [
            c for c in self.conversations.values()
            if isinstance(c, IM)
        ]

    @property
    def pm(self):
        return [
            c for c in self.conversations.values()
            if isinstance(c, MP)
        ]

    @property
    def conversations(self):
        @cache_disk(expire=36000)
        def _conversations():
            return {
                c.id: c
                for c in Groups().list()
                + Channels().list()
                + IMS().list()
                + MPIM().list()
            }
        return _conversations()


class UserType(ParameterType):
    def complete(self, ctx, incomplete):
        return [
            user["name"] for user in config.slack.users.values()
            if startswith(user["name"], incomplete)
        ]

    def convert(self, value, param, ctx):
        try:
            return [
                user for user in config.slack.users.values()
                if user["name"] == value
            ][0]
        except (ValueError, UnicodeError):
            self.fail('%s is not a valid user name' % value, param, ctx)


class ConversationType(ParameterType):
    def complete(self, ctx, incomplete):
        return [
            c.name for c in config.slack.conversations.values()
            if startswith(c.name, incomplete)
        ]

    def convert(self, value, param, ctx):
        try:
            return [
                c for c in config.slack.conversations.values()
                if c.name == value
            ][0]
        except (ValueError, UnicodeError):
            self.fail('%s is not a valid group name' % value, param, ctx)


@group()
@param_config(
    "slack", "--token",
    typ=SlackConfig,
    default=get_slack_token,
    required=True)
def slack():
    pass


@slack.command()
@table_fields(
    choices=[
        'id',
        "name",
    ],
    default=(
        "id",
        "name",
    )
)
@table_format()
def users(fields, format):
    with TablePrinter(fields, format) as tp:
        tp.echo_records(u.data for u in config.slack.users.values())


@slack.command()
@table_fields(
    choices=[
        'id',
        "name_fmt",
        "topic_fmt",
        'created_date',
        'creator_fmt',
        'members_fmt',
        'purpose_fmt',
        'num_members',
    ],
    default=(
        "name_fmt",
        "creator_fmt",
        "created_date",
        "num_members",
    )
)
@table_format()
@option("--type", type=click.Choice(["channels", "groups", "ims", "pmim"]))
def conversations(fields, format, type):
    vs = [
        v.data
        for v in config.slack.conversations.values()
        if not type or
        (type == "channels" and isinstance(v, Channel)) or
        (type == "groups" and isinstance(v, Group)) or
        (type == "ims" and isinstance(v, IM)) or
        (type == "pmim" and isinstance(v, MP))
    ]
    with TablePrinter(fields, format) as tp:
        tp.echo_records(vs)


@slack.command()
@table_fields(
    choices=[
        'datetime',
        "username",
        "text",
        "type",
    ],
    default=(
        'datetime',
        "username",
        "text",
    )
)
@table_format()
@argument("conversation", type=ConversationType())
@option("--oldest")
@option("--latest")
def history(fields, format, conversation, oldest, latest):
    if oldest:
        cal = parsedatetime.Calendar()
        oldest = cal.parseDT(oldest)[0].strftime("%s")
    if latest:
        cal = parsedatetime.Calendar()
        latest = cal.parseDT(latest)[0].strftime("%s")
    with TablePrinter(fields, format) as tp:
        tp.echo_records(
            message.data for message in conversation.history(
                oldest=oldest, latest=latest)
        )


@slack.command()
@option("--output-directory")
def record_log(output_directory):
    for c in config.slack.conversations.values():
        LOGGER.info("Dumping history of {}".format(c.data["name_fmt"]))
        output_directory_conversation = os.path.join(
            output_directory, c.data["name_fmt"])
        last_record_file = os.path.join(
            output_directory_conversation, ".oldest")
        if os.path.exists(last_record_file):
            with open(last_record_file) as f:
                oldest = f.read()
        else:
            oldest = None
        makedirs(output_directory_conversation)
        with open(os.path.join(
                output_directory_conversation, "messages.txt"),
                  "a",
                  encoding="utf-8"
        ) as message_file:
            now = datetime.datetime.now()
            for message in c.history(oldest=oldest):
                message_file.write("{} - {} : {}\n".format(
                    message.data["datetime"], message.data["username"],
                    message.data["text"])
                )
        with open(last_record_file, "w", encoding="utf-8") as f:
            f.write("{}".format(now.strftime("%s")))


@slack.command()
def ipython():
    c = config.slack.client
    s = config.slack
    import IPython
    dict_ = globals()
    dict_.update(locals())
    IPython.start_ipython(argv=[], user_ns=dict_)
