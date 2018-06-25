#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from __future__ import print_function, absolute_import

import time
import os
import re
import datetime
import builtins
import json
import pprint
import asyncio
import base64
from requests.exceptions import HTTPError

import asyncio_redis
import websockets
import click
import slacker
import parsedatetime
from redis_bot import lib as botlib

from click_project.decorators import (
    group,
    table_fields,
    table_format,
    argument,
    param_config,
    option,
    flag,
)
from click_project.log import get_logger
from click_project.lib import (
    get_authenticator,
    TablePrinter,
    ParameterType,
    makedirs
)
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
        self.date = datetime.datetime.fromtimestamp(float(data["ts"]))
        data["ts_str"] = "ts: " + data["ts"]
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
        return self.date < o.date


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
            builtins.sorted(
                config.slack.users[u]["name"] for u in self.data["members"]
            )
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

    def invite(self, user):
        self.endpoint.invite(self.id, user.id)

    def set_topic(self, topic):
        self.endpoint.set_topic(self.id, topic)

    def history(self, oldest=None, latest=None, user=None):
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
            for message in get_history_unpage(oldest=oldest, latest=latest)
            if not user or message.get("user") == user.id
        ])

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
        data["real_name"] = data.get("real_name", data["name"])
        data["display_name"] = data["profile"].get("display_name", data["name"])
        self.id = data["id"]
        self.name = data["name"]
        self.email = data["profile"].get("email")
        data["email"] = self.email

    def __getitem__(self, item):
        return self.data[item]

    def __hash__(self):
        return hash(self.id)

    def __repr__(self):
        return "<{}>".format(self.data["name"])


async_get = asyncio.get_event_loop().run_until_complete


class RTM():
    def __init__(self, data):
        self.data = data
        url = data["url"]
        self.ws = async_get(websockets.connect(url))
        assert async_get(self.read())["type"] == "hello"
        self.id = 0

    async def read(self):
        msg = await self.ws.recv()
        result = json.loads(msg)
        return result

    async def write(self, message):
        message = message.copy()
        message["id"] = self.id
        await self.ws.send(json.dumps(message))
        self.id = self.id + 1

    async def listen(
            self,
            conversations=None,
            types=None,
            user=None,
    ):
        while True:
            chunk = await self.read()
            if (
                    (
                        not conversations
                        or chunk.get("channel") in [
                            conversation.id
                            for conversation in conversations
                        ]
                    )
                    and
                    (
                        not types
                        or chunk["type"] in types
                    )
                    and
                    (
                        not user
                        or chunk.get("user") == user.id
                    )
            ):
                yield chunk


class SlackConfig():
    def __init__(self):
        self._client = None
        self._rtm = None
        self._users = None

    @property
    def me(self):
        return self.rtm.data["self"]

    @property
    def rtm(self):
        if self._rtm is None:
            tryit = True
            while tryit:
                try:
                    self._rtm = RTM(self.client.rtm.connect().body)
                except HTTPError as e:
                    if e.response.status_code == 429:
                        wait_time = int(e.response.headers["Retry-After"]) * 1.2
                        LOGGER.info(
                            "Got a 429 response while getting the RTM."
                            " Waiting for {}s".format(wait_time))
                        time.sleep(wait_time)
                    else:
                        raise
                else:
                    tryit = False
        return self._rtm

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
        if self._users is None:
            self._users = _users()
        return self._users

    @property
    def active_users(self):
        return {
            key: value
            for key, value in self.users.items()
            if (
                    not value.data.get("deleted")
                    and not value.data.get("is_bot")
                    and not value.data.get("real_name") == "slackbot"
            )
        }

    def get_user(self, name_or_id):
        return [
            u
            for u in self.users.values()
            if u.data["name"] == name_or_id
            or u.data["id"] == name_or_id
        ][0]

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
            if startswith(user.name, incomplete)
            or (user.email and startswith(user.email, incomplete))
            or startswith(user["real_name"], incomplete)
            or startswith(user["display_name"], incomplete)
        ]

    def convert(self, value, param, ctx):
        try:
            return [
                user for user in config.slack.users.values()
                if user.name == value
                or (user.email and user.email == value)
                or user["real_name"] == value
                or user["display_name"] == value
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
        "real_name",
        "email",
        "deleted",
        "is_bot",
    ],
    default=(
        "id",
        "name",
        "email",
    )
)
@table_format()
@option("--missing-in", type=ConversationType())
def users(fields, format, missing_in):
    users = config.slack.active_users
    if missing_in:
        keys = set(users) - set(missing_in.data["members"])
        users = {
            key: users[key]
            for key in keys
        }
    with TablePrinter(fields, format) as tp:
        tp.echo_records(u.data for u in users.values())


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
        "ts_str",
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
@option("--user", type=UserType())
def history(fields, format, conversation, oldest, latest,
            user):
    if oldest:
        cal = parsedatetime.Calendar()
        oldest = cal.parseDT(oldest)[0].strftime("%s")
    if latest:
        cal = parsedatetime.Calendar()
        latest = cal.parseDT(latest)[0].strftime("%s")
    with TablePrinter(fields, format) as tp:
        tp.echo_records(
            message.data for message in conversation.history(
                oldest=oldest, latest=latest, user=user)
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


@slack.group()
def rtm():
    ""


@rtm.command()
@argument("conversations", type=ConversationType(), nargs=-1)
@option("--type", "types", multiple=True)
@option("--user", type=UserType())
def listen(conversations, types, user):
    _listen = config.slack.rtm.listen

    async def do():
        async for message in _listen(
                conversations=conversations,
                types=types,
                user=user,
        ):
            pprint.pprint(message)
    async_get(do())


def mess_encode(mess):
    return botlib.mess_encode(mess)


def mess_decode(mess):
    return botlib.mess_decode(mess)


@rtm.command()
@option("--conversation", "conversations", type=ConversationType(), multiple=True)
@option("--type", "types", multiple=True)
@option("--redis-host", default="localhost")
@option("--redis-port", type=int, default=6379)
@option("--channel-to", default="bot:comm:to")
@option("--channel-control", default="bot:comm:control")
@option("--channel-from", default="bot:comm:from")
def redis_bot(conversations, types, channel_control, channel_to,
              channel_from, redis_host, redis_port):
    _listen = config.slack.rtm.listen

    redis_connection = botlib.ChanToRedis(
        redis_host, redis_port,
        channel_from, channel_to, channel_control
    )
    el = asyncio.get_event_loop()

    async def slack_listen():
        async for message in _listen(
                conversations=conversations,
                types=types
        ):
            def fixup_message(message):
                if "channel" in message:
                    message["mucroom"] = config.slack.get_conversation(
                        message["channel"]
                    ).name
                if "user" in message:
                    message["username"] = config.slack.get_user(
                        message["user"]
                    ).name
                message["body"] = message.get("text", "")
            try:
                fixup_message(message)
            except:
                import sys
                import ipdb
                ipdb.post_mortem(sys.exc_info()[2])

            LOGGER.debug("Got message: {}".format(message))
            mess = mess_encode(message)
            await redis_connection.send(mess)

    async def redis_listen():
        async for message in redis_connection.listen():
            if "channel" not in message and "mucroom" in message:
                message["channel"] = config.slack.get_conversation(message["mucroom"]).id
            if "text" not in message and "body" in message:
                message["text"] = message["body"]
            if "type" not in message:
                message["type"] = "message"
            LOGGER.debug("Write message: {}".format(message))
            await config.slack.rtm.write(message)

    asyncio.ensure_future(slack_listen())
    asyncio.ensure_future(redis_listen())
    el.run_forever()


@rtm.command()
@option("--conversation", type=ConversationType())
@argument("message")
def send(conversation, message):
    ""
    for user in config.slack.users.values():
        message = message.replace("@" + user.name, "<@{}>".format(user.id))
    async_get(
        config.slack.rtm.write(
            {
                "type": "message",
                "channel": conversation.id,
                "text": message,
            }
        )
    )


@slack.command()
def ipython():
    c = config.slack.client
    s = config.slack
    import IPython
    dict_ = globals()
    dict_.update(locals())
    IPython.start_ipython(argv=[], user_ns=dict_)


@slack.command()
@argument("name")
def create_group(name):
    resp = config.slack.groups.create(name).body
    LOGGER.info("Group created with id {}".format(resp["id"]))


@slack.command()
@option("--user", "users", type=UserType(), multiple=True)
@option("--all-but-user", "all_but_users", type=UserType(), multiple=True)
@argument("conversation", type=ConversationType())
def invite_to_conversation(users, all_but_users, conversation):
    assert (
        (users and not all_but_users)
        or (all_but_users and not users)
    )
    users_to_invite = []
    if users:
        users_to_invite = list(users)
    if all_but_users:
        users_to_invite = set(config.slack.users.values()) - set(all_but_users)
    for user in users_to_invite:
        conversation.invite(user)


@slack.command()
@argument("conversation", type=ConversationType())
@argument("topic")
def set_conversation_topic(conversation, topic):
    conversation.set_topic(topic)


@slack.command()
@argument("user", type=UserType(), nargs=-1)
def create_mpim(user):
    config.slack.client.mpim.open([u.id for u in user])
