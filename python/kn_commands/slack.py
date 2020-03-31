#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from __future__ import print_function, absolute_import

import time
import os
import re
import datetime
import sys
import builtins
from dateutil.parser import parse as dateparse
import json
import requests
import pprint
import asyncio
import redis
from requests.exceptions import HTTPError

import websockets
import click
import slacker
import parsedatetime
try:
    from redis_bot import lib as botlib
except ImportError:
    botlib = None

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
    makedirs,
    json_dumps,
    createfile,
)
from click_project.completion import startswith
from click_project.config import config
from click_project.core import cache_disk
from click_project.commands.passwords import set as password_set_command


LOGGER = get_logger(__name__)

db = redis.StrictRedis(decode_responses=True)


def get_slack_token():
    values = get_authenticator(config.slack.account + "_slack_token", required=False, askpass=False)
    if values is not None:
        return values[1]
    else:
        return None


def parse_text(text):
    text = text.replace("{", "{{").replace("}", "}}")
    text = re.sub("<@([^>|]+)([|][^>]+)?>", r"{\1}", text)
    text = text.format(
        **{
            id: "@" + user["name"]
            for id, user in config.slack.users.items()
        }
    )
    return text


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
        data["text"] = parse_text(data["text"])
        self.date = datetime.datetime.fromtimestamp(float(data["ts"]))
        data["ts_str"] = "ts: " + data["ts"]
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
        self.data["name_fmt"] = self.name
        self.name_fmt = self.name
        self.record_logs_dir = (
            os.path.join(config.slack.records, self.name)
            if config.slack.records else None
        )
        self.recorded_messages = (
            os.path.join(self.record_logs_dir, "messages.json")
            if config.slack.records else None
        )
        self.last_record_file = (
            os.path.join(self.record_logs_dir, ".oldest")
            if config.slack.records else None
        )
        self.archived_file = (
            os.path.join(self.record_logs_dir, ".archived")
            if config.slack.records else None
        )

    @property
    def archived(self):
        return (
            self.archived_file is not None
            and
            os.path.exists(self.archived_file)
        )

    def records(self,
                oldest=None, latest=None, user=None, count=100000):
        if (
                config.slack.records is None
                or
                not os.path.exists(self.recorded_messages)
        ):
            return []
        with open(self.recorded_messages, "r") as recordfile:
            messages = [
                Message(json.loads(line))
                for line in recordfile
            ]
            return [
                m for m in messages
                if (
                        (
                            oldest is None
                            or oldest <= m.date
                        )
                        and (
                            latest is None
                            or m.date <= latest
                        )
                        and (
                            user is None
                            or m.get("user") == user.id
                        )
                )
            ]

    def __eq__(self, other):
        return self.id == other.id

    def __getitem__(self, name):
        return getattr(self, name)

    def leave(self):
        if config.dry_run:
            LOGGER.info(f"Would leave {self.name}")
        else:
            self.endpoint.leave(self.id)

    def archive(self):
        createfile(self.archived_file, str(datetime.datetime.now()))

    @property
    def members(self):
        @cache_disk(expire=36000)
        def _conversation_members(id, token):
            return config.slack.client.conversations.members(id).body["members"]
        return _conversation_members(self.id, config.slack.token)

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
                config.slack.users[u]["name"] for u in self.members
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
        try:
            self.endpoint.invite(self.id, user.id)
        except slacker.Error as e:
            LOGGER.error("Could not invite {}".format(user.name))
            raise

    def set_topic(self, topic):
        self.endpoint.set_topic(self.id, topic)

    def history(self,
                oldest=None, latest=None, user=None, count=1000,
                record=False, combine=True, only_new=False,
                with_subtypes=True):
        if isinstance(oldest, datetime.datetime):
            oldest = oldest.timestamp()
        if isinstance(latest, datetime.datetime):
            latest = latest.timestamp()
        records = self.records(
            oldest=oldest, latest=latest, user=user, count=count)
        if record:
            hist = records
        else:
            if combine and not only_new:
                if count is not None:
                    count = count - len(records)
                if records:
                    oldest = records[-1].data["ts"]
            elif only_new:
                oldest = records[-1].data["ts"] if records else None
            hist_ = self._history(
                oldest=oldest, latest=latest, user=user, count=count)
            if combine and not only_new:
                hist = records + hist_
            else:
                hist = hist_
        return [
            m for m in hist
            if with_subtypes or "subtype" not in m.data
        ]

    def _history(self, oldest=None, latest=None, user=None, count=1000):
        def get_history_unpage(latest, oldest, count):
            res = self.endpoint.history(
                self.data["id"],
                count=count,
                latest=latest,
                oldest=oldest,
            ).body
            messages = res["messages"]
            if res["has_more"]:
                messages += get_history_unpage(latest=messages[-1]["ts"],
                                               oldest=oldest,
                                               count=count,
                )
            return messages
        return builtins.sorted([
            Message(message)
            for message in get_history_unpage(
                    oldest=oldest, latest=latest,
                    count=count
            )
            if not user or message.get("user") == user.id
        ])

    def __repr__(self):
        return "<{} - {}>".format(self.data["id"], self.name)

    def get_message(self, timestamp):
        return Message(config.slack.client.reactions.get(
            channel=self.id,
            timestamp=timestamp,
        ).body["message"])

    def delete_message(self, message):
        config.slack.client.chat.delete(
            self.id,
            message.data["ts"],
        )


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
        @cache_disk(expire=36000)
        def _list(token, name):
            return [
                self.cls(self.endpoint, c)
                for c in self.endpoint.list().body[self.list_index]
            ]
        return _list(config.slack.token, self.__class__.__name__)

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
    def __init__(self, endpoint):
        self.endpoint = endpoint
        async_get(self.reset())

    async def reset(self):
        attempt = 0
        max_attempts = 10
        self.data = None
        while self.data is None:
            attempt += 1
            if attempt == max_attempts:
                raise RuntimeError(f"Could not connect, tried {attempt} times")
            try:
                self.data = self.endpoint.connect().body
            except requests.exceptions.ConnectionError:
                LOGGER.info("Connection closed, reconnecting")
                time.sleep(1)

        url = self.data["url"]
        self.ws = await websockets.connect(url)
        hello = await self.read()
        assert hello["type"] == "hello", f"{hello} must be of type hello"
        self.id = 0

    async def read(self):
        msg = None
        attempt = 0
        max_attempts = 10
        while msg is None:
            attempt += 1
            if attempt == max_attempts:
                raise RuntimeError(f"Could not connect, tried {attempt} times")
            try:
                msg = await self.ws.recv()
            except websockets.exceptions.ConnectionClosed:
                LOGGER.info("Connection closed, reconnecting")
                await self.reset()
                time.sleep(1)
        result = json.loads(msg)
        return result

    async def write(self, message):
        message = message.copy()
        self.id = self.id + 1
        message["id"] = self.id
        res = await self.ws.send(json.dumps(message))
        return res

    async def listen(
            self,
            conversations=None,
            types=None,
            subtypes=None,
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
                        or chunk.get("item", {}).get("channel") in [
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
                        not subtypes
                        or chunk.get("subtype") in subtypes
                    )
                    and
                    (
                        not user
                        or chunk.get("user") == user.id
                    )
            ):
                if "user" in chunk:
                    chunk["username"] = config.slack.users[chunk["user"]]["name"]
                if "item_user" in chunk:
                    chunk["item_username"] = (
                        config.slack.users[chunk["item_user"]]["name"]
                    )
                if "text" in chunk:
                    chunk["parsed_text"] = parse_text(chunk["text"])
                if "channel" in chunk:
                    chunk["channelname"] = (
                        config.slack.get_conversation(chunk["channel"]).name
                    )
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
    def me_user(self):
        return self.users[self.rtm.data["self"]["id"]]

    @property
    def rtm(self):
        if self._rtm is None:
            tryit = True
            while tryit:
                try:
                    self._rtm = RTM(self.client.rtm)
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
        def _users(token):
            return {
                user["id"]: User(user)
                for user in self.client.users.list().body["members"]
            }
        if self._users is None:
            self._users = _users(self.token)
        return self._users

    @property
    def active_users(self):
        return {
            key: value
            for key, value in self.users.items()
            if (
                    not value.data.get("deleted")
                    and not value.data.get("is_bot")
                    and not value.data.get("real_name").lower() == "slackbot"
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
    def all_conversations(self):
        return {
            c.id: c
            for c in Groups().list()
            + Channels().list()
            + IMS().list()
            + MPIM().list()
        }

    @property
    def conversations(self):
        return {
            k: v
            for k, v in self.all_conversations.items()
            if not v.archived
        }

    @property
    def archived_conversations(self):
        return {
            k: v
            for k, v in self.all_conversations.items()
            if v.archived
        }


def find_user(value):
    return [
        user for user in config.slack.users.values()
        if user.name == value
        or (user.email and user.email == value)
        or user["real_name"] == value
        or user["display_name"] == value
    ][0]


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
            return find_user(value)
        except (ValueError, UnicodeError):
            self.fail('%s is not a valid user name' % value, param, ctx)


class ConversationType(ParameterType):
    def __init__(self, all=False, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.all = all

    @property
    def completer(self):
        if self.all:
            return config.slack.all_conversations
        else:
            return config.slack.conversations

    def complete(self, ctx, incomplete):
        return [
            c.name for c in self.completer.values()
            if startswith(c.name, incomplete)
        ]

    def convert(self, value, param, ctx):
        def fail():
            self.fail('%s is not a valid group name' % value, param, ctx)
        try:
            return [
                c for c in self.completer.values()
                if c.name == value
            ][0]
        except IndexError as e:
            try:
                user = find_user(value)
                channel_id = config.slack.client.im.open(user.id).body["channel"]["id"]
                return IMS().get(channel_id)
            except IndexError:
                fail()
        except (ValueError, UnicodeError):
            fail()


@group()
@param_config(
    "slack", "--account",
    typ=SlackConfig,
    default="default",
    required=True,
    help="The authenticator key to use to get the token")
@param_config(
    "slack", "--token",
    typ=SlackConfig,
    default=get_slack_token,
    required=True,
    help="The token provided by slack")
@param_config(
    "slack", "--records", help="therecords")
def slack():
    """Play with slack

    Talk to people, record the history, etc."""


def message_handle(message):
    for user in config.slack.users.values():
        message = message.replace("@" + user.name, "<@{}>".format(user.id))
    return message


class MessageType(ParameterType):
    def convert(self, value, param, ctx):
        return message_handle(value)


@slack.command()
def save_token():
    """Save the given token in the keyring, associated to the given account"""
    ctx = click.get_current_context()
    ctx.invoke(
        password_set_command,
        machine=config.slack.account + "_slack_token",
        username="not used",
        password=config.slack.token,
    )


@slack.command()
@argument("conversation", type=ConversationType(),
        help="The conversation of interest")
@option("--reaction",
        multiple=True,
        help="Some reactions to put in it."
        " Implies --wait-server-response")
@flag("--me/--no-me", help="Send a me message")
@option("--thread", help="Reply in a thread")
@argument("message", help="The message to send", type=MessageType())
def say(conversation, message, reaction, me, thread):
    """Post a message to this conversation"""
    endpoint = (
        config.slack.client.chat.me_message
        if me
        else config.slack.client.chat.post_message
    )
    kwargs = {}
    if thread:
        kwargs["thread_ts"] = thread
    response = endpoint(conversation.id, message, **kwargs)
    for r in reaction:
        config.slack.client.reactions.add(
            channel=conversation.id,
            timestamp=response.body["ts"],
            name=r,
        )


slack.add_command(say, "post-message")


@slack.command()
@argument("conversation", type=ConversationType(),
          help="The conversation of interest")
@argument("timestamp",
          help="The timestamp of the message")
def delete(conversation, timestamp):
    """Delete a message from this conversation"""
    config.slack.client.chat.delete(
        conversation.id,
        timestamp,
    )


@slack.command()
@argument("conversation", type=ConversationType(),
          help="The conversation of interest")
@argument("text",
          help="The text of the message")
def me_message(conversation, text):
    """Delete a message from this conversation"""
    config.slack.client.chat.me_message(
        conversation.id,
        text,
    )


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
@option("--missing-in", type=ConversationType(), help="Only those that are not there")
@option("--in", "ins", type=ConversationType(), help="Only those that are there")
def users(fields, format, missing_in, ins):
    """Display the users"""
    users = config.slack.active_users
    if missing_in:
        keys = set(users) - set(missing_in.members)
        users = {
            key: users[key]
            for key in keys
        }
    if ins:
        keys = set(users).intersection(set(ins.members))
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
@option("--type", type=click.Choice(["channels", "groups", "ims", "pmim"]),
        help="What kind of conversation to dump")
@flag("--archived", help="Only archived ones")
@flag("--all", help="All of them")
def conversations(fields, format, type, archived, all):
    """Dump all conversations"""
    vs = [
        v
        for v in (
                config.slack.archived_conversations if archived else
                config.slack.all_conversations if all else
                config.slack.conversations
        ).values()
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
        "ts_str",
        'datetime',
        "username",
        "text",
    )
)
@table_format()
@argument("conversation", type=ConversationType(all=True),
          help="The conversation to get the history from")
@option("--oldest", help="The oldest timestamp to consider")
@option("--latest", help="The latest timestamp to consider")
@option("--count", help="The number of messages to show")
@option("--user", type=UserType(), help="Show only messages from this user")
@flag("--record", help="Dump records instead of the history")
@flag("--combine/--no-combine", help="Combine records and history",
      default=True)
@flag("--only-new", help="Show only new messages (not in the record)")
@flag("--with-subtypes/--without-subtypes", help="Show the subtypes also", default=True)
def history(fields, format, conversation, oldest, latest,
            count, record, user, combine, only_new, with_subtypes):
    """Show the history of messages from a conversation"""
    if oldest:
        cal = parsedatetime.Calendar()
        oldest = cal.parseDT(oldest)[0].strftime("%s")
    if latest:
        cal = parsedatetime.Calendar()
        latest = cal.parseDT(latest)[0].strftime("%s")
    with TablePrinter(fields, format) as tp:
        tp.echo_records(message.data for message in conversation.history(
            oldest=oldest, latest=latest, user=user, count=count,
            record=record, combine=combine, only_new=only_new,
            with_subtypes=with_subtypes
        ))


@slack.command()
@flag("--archived", help="Only archived ones")
@flag("--all", help="All of them")
def record_log(archived, all):
    """Dump the records for archiving purpose."""
    for c in (
            config.slack.archived_conversations if archived else
            config.slack.all_conversations if all else
            config.slack.conversations
    ).values():
        LOGGER.info("Dumping history of {}".format(c.data["name_fmt"]))
        makedirs(c.record_logs_dir)
        with open(c.recorded_messages, "a", encoding="utf-8") as message_file:
            for message in c.history(only_new=True):
                message_file.write(json.dumps(message.data))
                message_file.write("\n")


@slack.group()
def rtm():
    """Real Time Chat"""


@rtm.command()
@argument("conversations", type=ConversationType(), nargs=-1,
          help="The conversation to listen to")
@option("--type", "types", multiple=True, help="The type of message to let through")
@option("--subtype", "subtypes", multiple=True, help="The sub-type of message to let through")
@option("--user", type=UserType(), help="Show only messages from this user")
@option("--field", "fields", multiple=True, help="Show only this field")
def listen(conversations, types, user, fields, subtypes):
    """Listen to slack event in a conversation

    The events are printed as-is, allowing you to pipe them for further
    processing.

    """
    _listen = config.slack.rtm.listen

    async def do():
        async for message in _listen(
                conversations=conversations,
                types=types,
                subtypes=subtypes,
                user=user,
        ):
            if fields:
                print("|".join(message[field] for field in fields))
            else:
                pprint.pprint(message)
            sys.stdout.flush()
    async_get(do())


if botlib is not None:
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
@option("--conversation", type=ConversationType(),
        help="The conversation of interest")
@option("--reaction",
        multiple=True,
        help="Some reactions to put in it."
        " Implies --wait-server-response")
@argument("message", help="The message to send", type=MessageType())
@flag("--wait-server-response/--no-wait-server-response", default=True,
      help="Wait for the server to acknowledge the message")
def send(conversation, message, reaction, wait_server_response):
    return _send(conversation, message, reaction, wait_server_response)


def _send(conversation, message, reaction, wait_server_response):
    """Send a message to this conversation"""
    wait_server_response = reaction or wait_server_response
    async_get(
        config.slack.rtm.write(
            {
                "type": "message",
                "channel": conversation.id,
                "text": message,
            }
        )
    )
    response = None
    if wait_server_response:
        LOGGER.info("Message sent, waiting for server response")
        response = async_get(config.slack.rtm.read())
        LOGGER.debug("Got response: {}".format(json_dumps(response)))
        while response.get("reply_to") != config.slack.rtm.id:
            response = async_get(config.slack.rtm.read())
            LOGGER.debug("Got response: {}".format(json_dumps(response)))
        LOGGER.info(json_dumps(response))
    for r in reaction:
        config.slack.client.reactions.add(
            channel=conversation.id,
            timestamp=response["ts"],
            name=r,
        )
    return response


@slack.command()
def ipython():
    """Start an interactive python session

    With c = the client and s = the slack object
    """
    c = config.slack.client
    s = config.slack
    import IPython
    dict_ = globals()
    dict_.update(locals())
    IPython.start_ipython(argv=[], user_ns=dict_)


@slack.command()
@argument("name")
def create_group(name):
    resp = config.slack.client.groups.create(name).body
    LOGGER.info("Group created with id {}".format(resp["group"]["id"]))


@slack.command()
@option("--user", "users", type=UserType(), multiple=True,
        help="Invite those users")
@option("--all-but-user", "all_but_users", type=UserType(), multiple=True,
        help="Don't invite those users")
@flag("--all")
@flag("--only-active-users/--also-inactive-users",
      default=True,
      help="Invite only the really active users")
@argument("conversation", type=ConversationType(), help="The conversation")
def invite_to_conversation(users, all, all_but_users, conversation, only_active_users):
    """Invite those users in the conversation"""
    assert (
        all or
        (users and not all_but_users)
        or (all_but_users and not users)
    )
    users_to_invite = []
    all_users = (
        config.slack.active_users
        if only_active_users else config.slack.users
    )
    if users:
        users_to_invite = list(users)
    if all_but_users:
        all_but_users += config.slack.users[config.slack.me["id"]],
        users_to_invite = set(all_users.values()) - set(all_but_users)
    if all:
        users_to_invite = all_users.values()
    # don't invite already present users
    users_to_invite = [
        user
        for user in users_to_invite
        if user["id"] not in conversation.members
    ]
    for user in users_to_invite:
        LOGGER.info("Inviting {}".format(user["name"]))
        try:
            conversation.invite(user)
        except slacker.Error as e:
            if e.args[0] == "already_in_channel":
                LOGGER.warning("{} already in channel".format(user["name"]))
            else:
                raise e


@slack.command(handle_dry_run=True)
@option("--inactivity",
        help="How much time for a conversation must be inactive"
        " to leave it (xYxMxD)",
        default="4M")
@flag("--alone/--not-alone", default=True, help="Also leave groups where I am alone")
@option("--keep", multiple=True, type=ConversationType(),
        help="Keep this conversation, even if it matches the criteria.")
@flag("--ask/--dont-ask", default=True, help="Interactively ask for each conversation")
@flag("--with-subtypes/--without-subtypes", help="Show the subtypes also")
def auto_archive(inactivity, alone, keep, ask, with_subtypes):
    """Leave groups and MPIM where nothing is expected to happen soon."""
    now = datetime.datetime.now()
    time_dict = re.match(
        "^\s*((?P<years>\d+)\s*Y)?"
        "\s*((?P<months>\d+)\s*M?)?"
        "\s*((?P<days>d\+)\s*D)?\s*$",
        inactivity
    ).groupdict()
    years = int(time_dict['years'] or "0")
    months = int(time_dict['months'] or "0")
    days = int(time_dict['days'] or "0") + 30 * months + 365 * years
    duration = datetime.timedelta(days=days)

    def matching_conversations():
        for conversation in config.slack.conversations.values():
            LOGGER.debug(f"## Scanning conversation {conversation.name}")
            hist = conversation.history(count=1, with_subtypes=with_subtypes)
            if len(conversation.members) == 1 and alone:
                LOGGER.info(f"Empty conversation {conversation.name}")
                yield conversation
            elif hist and (hist[-1].date + duration) < now:
                LOGGER.info(f"Inactive conversation {conversation.name} ({hist[-1].date})")
                yield conversation
            elif not(hist):
                LOGGER.info(f"Abandoned conversation {conversation.name}")
                yield conversation

    for conversation in matching_conversations():
        if conversation in keep:
            LOGGER.info(f"Unconditionally keep {conversation.name}")
        elif (
                not ask
                or click.confirm(
                    f"Really archive {conversation.name} ({len(conversation.members)})\n(use --dont-ask to avoid asking this)?"
                )
        ):
            conversation.archive()


@slack.command()
@argument("conversation", type=ConversationType(), help="The conversation")
@argument("topic", help="The topic")
def set_conversation_topic(conversation, topic):
    """Set the topic of the conversation"""
    conversation.set_topic(topic)


@slack.command()
@argument("user", type=UserType(), nargs=-1, help="The user to invite")
def create_mpim(user):
    """Create a conversation for multiple users"""
    config.slack.client.mpim.open([u.id for u in user])


@slack.command()
@argument("user", type=UserType(), nargs=-1, help="The user to discuss with")
def create_im(user):
    """Start single conversations"""
    for u in user:
        config.slack.client.im.open(u.id)


@slack.command()
@argument("conversation", type=ConversationType(),
          help="The conversation to scan")
@argument("url", help="URL of the feed")
@argument("title", help="Title of the feed")
@option("--subtitle", help="Subtitle of the feed")
@option("--language", default="en", help="Language of the feed")
@argument("author-name", help="The name of the author of the feed")
@argument("author-email", help="The email of the author")
@argument("output-path", help="Where to put the RSS feed")
def rss(conversation,
        url,
        author_name,
        author_email,
        title,
        subtitle,
        language,
        output_path):
    """Export all the links of the conversation in a simple RSS feed"""
    from feedgen.feed import FeedGenerator
    from konix_time_helper import naive_to_local
    fg = FeedGenerator()
    fg.id(url)
    fg.title(title)
    fg.author(
        {
            'name': author_name,
            'email': author_email,
        }
    )
    fg.link(
        href=url,
        rel='alternate'
    )
    if subtitle:
        fg.subtitle(subtitle)
    fg.language(language)
    for message in conversation.history():
        match = re.search(
            "^.*<(?P<url>[^>|]+)\|?(?P<title>[^>]+)?>.*$",
            message.data["text"],
            flags=re.MULTILINE
        )
        if match is not None:
            fe = fg.add_entry()
            link = match.group("url")
            title = match.group("title") or link
            date = naive_to_local(datetime.datetime.fromtimestamp(float(message.data["ts"])))
            description = message.data["text"]
            if "attachments" in message.data:
                attachment = [a for a in message.data["attachments"] if
                              a["title_link"] == link][0]
                title += " | " + attachment["title"]
                description += """

""" + attachment["text"]
            fe.id(link)
            fe.title(title)
            fe.link(href=link)
            fe.published(date)
            user = config.slack.get_user(message.data["user"])
            author = {
                "name": message.data["username"],
                "email": user.email or "noemail",
            }
            fe.author(author)
            fe.description(description)
    fg.rss_file(output_path, pretty=True)


class SlackReaction():
    pass


@slack.group()
@param_config(
    "slack_reaction", "--conversation",
    typ=SlackReaction,
    required=True,
    type=ConversationType(),
    help="The conversation with the message to react on",
)
@param_config(
    "slack_reaction", "--timestamp",
    typ=SlackReaction,
    required=True,
    help="The timestamp of the message to react on",
)
def reaction():
    """Handle reactions on a message"""


@reaction.command()
@argument("reaction", nargs=-1, help="The reaction to add")
def add(reaction):
    """Add a reaction to this message"""
    for r in reaction:
        config.slack.client.reactions.add(
            channel=config.slack_reaction.conversation.id,
            timestamp=config.slack_reaction.timestamp,
            name=r,
        )


def reaction_handle(reaction):
    return {
        **reaction,
        "users_txt": [
            config.slack.users[u]["display_name"]
            for u in reaction["users"]
        ]
    }


class MessageReactionType(ParameterType):
    @staticmethod
    def list():
        return [
            reaction_handle(reaction)
            for reaction in config.slack.client.reactions.get(
                channel=config.slack_reaction.conversation.id,
                timestamp=config.slack_reaction.timestamp,
            ).body["message"].get("reactions", [])
        ]

    def complete(self, ctx, incomplete):
        return [
            reaction["name"]
            for reaction in self.list()
        ]

    def convert(self, value, param, ctx):
        try:
            return [
                reaction["name"]
                for reaction in self.list()
            ][0]
        except (ValueError, UnicodeError):
            self.fail('%s is not a valid user name' % value, param, ctx)


@reaction.command()
@argument("reaction", nargs=-1, help="The reaction to remove",
          type=MessageReactionType())
def remove(reaction):
    """Remove a reaction from this message"""
    for r in reaction:
        config.slack.client.reactions.remove(
            channel=config.slack_reaction.conversation.id,
            timestamp=config.slack_reaction.timestamp,
            name=r,
        )


@reaction.command()
@table_fields(
    choices=[
        "name",
        "users_txt",
        "count",
    ],
)
@table_format()
def show(fields, format):
    """Remove a reaction from this message"""
    message = config.slack.client.reactions.get(
        channel=config.slack_reaction.conversation.id,
        timestamp=config.slack_reaction.timestamp,
    ).body["message"]
    reactions = message.get("reactions", [])
    print(message["text"])
    with TablePrinter(fields, format) as tp:
        tp.echo_records(
            sorted(
                sorted(
                    map(reaction_handle, reactions),
                    key=lambda e: e["name"]
                ),
                key=lambda e: e["count"],
                reverse=True,
            )
        )


@slack.command()
@argument("conversation", type=ConversationType(),
          help="The conversation of interest")
@argument("timestamp",
          help="The timestamp of the message")
@argument("text", required=False,
          help="The text to use. If not given, an editor will be opened")
def edit(conversation, timestamp, text):
    """Edit the text of a message"""
    text = text or click.edit(conversation.get_message(timestamp).data["text"])
    if not text:
        LOGGER.info("Aboooort!")
        return
    config.slack.client.chat.update(
        channel=conversation.id,
        text=text,
        ts=timestamp,
    )


@reaction.command()
@option("--result-timestamp",
        help="Where to show the results. "
        "In the original message if left empty.")
@option("--ignore-reaction", multiple=True,
        help="Ignore those reactions in the contest")
def poll_update(result_timestamp, ignore_reaction):
    """Update the text of the message to indicate the winning emoji"""
    message = config.slack.client.reactions.get(
        channel=config.slack_reaction.conversation.id,
        timestamp=config.slack_reaction.timestamp,
    ).body["message"]
    result_message = config.slack.client.reactions.get(
        channel=config.slack_reaction.conversation.id,
        timestamp=result_timestamp or config.slack_reaction.timestamp,
    ).body["message"]
    reactions = [
        r for r in message.get("reactions", [])
        if r["name"] not in ignore_reaction
    ]
    try:
        max_count = max(map(lambda e: e["count"], reactions))
    except ValueError:
        winner = """"no one yet" """
    else:
        winners = [
            r for r in reactions
            if r["count"] == max_count
        ]
        if winners:
            if len(winners) > 3:
                winner = "no clear winner yet. "
            else:
                winner = " or ".join(
                    [
                        ":{}: ({})".format(winner["name"], winner["count"])
                        for winner in winners
                    ]
                )
        else:
            winner = """"no one yet" """
    text = result_message["text"]
    prefix = "Here comes the result:"
    winner_text = "{} {}".format(prefix, winner)
    if prefix in text:
        text = re.sub("{} .+".format(prefix), winner_text, text)
    else:
        text += winner_text
    config.slack.client.chat.update(
        channel=config.slack_reaction.conversation.id,
        text=text,
        ts=result_timestamp or config.slack_reaction.timestamp,
    )


@reaction.command()
@option("--result-timestamp",
        help="Where to show the results. "
        "In the original message if left empty.")
@option("--ignore-reaction", multiple=True,
        help="Ignore those reactions in the contest")
def poll_listen(result_timestamp, ignore_reaction):
    """Update the poll message when a reaction occurs"""
    ctx = click.get_current_context()
    _listen = config.slack.rtm.listen

    async def do():
        async for chunk in _listen(
                conversations=[config.slack_reaction.conversation],
                types=["reaction_added", "reaction_removed"],
                user=None,
                ):
            if chunk["item"]["ts"] == config.slack_reaction.timestamp:
                ctx.forward(poll_update)
    async_get(do())


class SlackPoll():
    pass


@slack.group()
@param_config(
    "slack_poll", "--conversation",
    typ=SlackPoll,
    required=True,
    type=ConversationType(),
    help="The conversation with the message to react on",
)
def poll():
    """Create and manipulate polls, using emojis"""


@poll.command()
@argument("question", help="The question to ask in the poll")
@option("--choice", multiple=True, nargs=2,
        help="The choice and the emoji to use")
@option("--dummy-choice", multiple=True, nargs=2,
        help="The dummy choice and the emoji to use"
        " (they won't be part of the poll)")
def create(question, choice, dummy_choice):
    """Create the poll"""
    message = question + "\n"
    for text, emoji in choice:
        message += ":{}:: {}\n".format(emoji, text)
    for text, emoji in dummy_choice:
        message += ":{}:: {}\n".format(emoji, text)
    response = config.slack.client.chat.post_message(
        config.slack_poll.conversation.id,
        message,
    ).body
    for text, r in choice:
        config.slack.client.reactions.add(
            channel=config.slack_poll.conversation.id,
            timestamp=response["ts"],
            name=r,
        )
    for text, r in dummy_choice:
        config.slack.client.reactions.add(
            channel=config.slack_poll.conversation.id,
            timestamp=response["ts"],
            name=r,
        )
    db.set(
        "slack.poll.{}".format(config.slack_poll.conversation.id),
        json.dumps(
            {
                "ts": response["ts"],
                "assoc": {
                    r: text
                    for text, r in choice
                },
                "dummy_assoc": {
                    r: text
                    for text, r in dummy_choice
                }
            }
        )
    )


@poll.command()
def update():
    """Update the poll message"""
    info = json.loads(db.get("slack.poll.{}".format(
        config.slack_poll.conversation.id)))
    message = config.slack.client.reactions.get(
        channel=config.slack_poll.conversation.id,
        timestamp=info["ts"],
    ).body["message"]
    reactions = [
        r for r in message.get("reactions", [])
        if r["name"] in list(info["assoc"].keys())
    ]
    try:
        max_count = max(map(lambda e: e["count"], reactions))
    except ValueError:
        winner = """"no one yet" """
    else:
        winners = [
            r for r in reactions
            if r["count"] == max_count
        ]
        if winners:
            if len(winners) > 3:
                winner = "no clear winner yet. "
            else:
                winner = " or ".join(
                    [
                        info["assoc"][winner["name"]]
                        for winner in winners
                    ]
                )
        else:
            winner = """"no one yet" """
    text = message["text"]
    prefix = "Here comes the result:"
    winner_text = "{} {}".format(prefix, winner)
    if prefix in text:
        text = re.sub("{} .+".format(prefix), winner_text, text)
    else:
        text += winner_text
    config.slack.client.chat.update(
        channel=config.slack_poll.conversation.id,
        text=text,
        ts=info["ts"],
    )


@poll.command()
def _listen():
    """Update the poll message when a reaction occurs"""
    info = json.loads(db.get("slack.poll.{}".format(
        config.slack_poll.conversation.id)))
    ctx = click.get_current_context()
    _listen = config.slack.rtm.listen

    async def do():
        async for chunk in _listen(
                conversations=[config.slack_poll.conversation],
                types=["reaction_added", "reaction_removed"],
                user=None,
                ):
            if chunk["item"]["ts"] == info["ts"]:
                ctx.invoke(update)
    async_get(do())


@poll.command()
def _clean():
    info = json.loads(db.get("slack.poll.{}".format(
        config.slack_poll.conversation.id)))
    config.slack.client.chat.delete(
        config.slack_poll.conversation.id,
        info["ts"],
    )
    db.delete("slack.poll.{}".format(
        config.slack_poll.conversation.id))


@poll.command()
def _exists():
    has_entry_in_db = db.get(
        "slack.poll.{}".format(config.slack_poll.conversation.id)
    )
    if has_entry_in_db is not None:
        entry = json.loads(has_entry_in_db)
        try:
            message = config.slack_poll.conversation.get_message(
                entry["ts"]
            )
        except slacker.Error as e:
            if e.args[0] == "message_not_found":
                db.delete(
                    "slack.poll.{}".format(config.slack_poll.conversation.id)
                )
                has_entry_in_db = None
            else:
                raise
    exit(
        1 if (has_entry_in_db is None)
        else 0
    )


@poll.command()
def _get():
    """Get the result of the poll"""
    info = json.loads(db.get("slack.poll.{}".format(
        config.slack_poll.conversation.id)))
    message = config.slack.client.reactions.get(
        channel=config.slack_poll.conversation.id,
        timestamp=info["ts"],
    ).body["message"]
    reactions = [
        r for r in message.get("reactions", [])
        if r["name"] in list(info["assoc"].keys())
    ]
    try:
        max_count = max(map(lambda e: e["count"], reactions))
    except ValueError:
        winner = None
    else:
        winners = [
            r for r in reactions
            if r["count"] == max_count
        ]
        if winners:
            if len(winners) > 3:
                winner = "no clear winner yet. "
            else:
                winner = " or ".join(
                    [
                        info["assoc"][winner["name"]]
                        for winner in winners
                    ]
                )
        else:
            winner = None
    if winner is not None:
        print(winner)
    else:
        sys.exit(1)


@slack.group()
def status():
    """Manipulate the status"""


@status.command()
@table_fields(
    choices=['title', 'phone', 'skype', 'real_name', 'real_name_normalized',
             'display_name', 'display_name_normalized', 'fields', 'status_text',
             'status_emoji', 'status_expiration', 'avatar_hash', 'email',
             'first_name', 'last_name', 'image_24', 'image_32', 'image_48',
             'image_72', 'image_192', 'image_512', 'status_text_canonical',
             "presence",
    ],
    default=[
        "real_name", "presence", "status_text", "status_emoji",
    ]
)
@table_format()
def get(fields, format):
    """Get the status"""
    presence = config.slack.client.users.get_presence(
        config.slack.me["id"]
    ).body["presence"]
    with TablePrinter(fields, format) as tp:
        tp.echo_records(
            [
                {
                    "presence": presence,
                    **config.slack.client.users.profile.get().body["profile"]
                }
            ]
        )


@status.command()
@option("--text", help="The text of the status")
@option("--emoji", help="The emoji of the status")
def _set(text, emoji):
    """Set the text or the emoji of the status"""
    if text is None and emoji is None:
        LOGGER.info("No change of status")
        return
    if emoji and not emoji.startswith(":"):
        emoji = ":" + emoji
    if emoji and not emoji.endswith(":"):
        emoji = emoji + ":"

    profile = {}
    if text is not None:
        profile["status_text"] = text
    if emoji is not None:
        profile["status_emoji"] = emoji
    config.slack.client.users.profile.set(
        profile=json.dumps(profile)
    )


@status.command()
def unset():
    """Remove the text and the emoji from the status"""
    ctx = click.get_current_context()
    ctx.invoke(_set, text="", emoji="")


@slack.command()
@option("--text", help="The text of the status")
@option("--emoji", help="The emoji of the status")
def leave(text, emoji):
    """Leave for some time"""
    config.slack.client.users.set_presence("away")
    ctx = click.get_current_context()
    ctx.forward(_set)


@slack.command()
@option("--text", help="The text of the status")
@option("--emoji", help="The emoji of the status")
def back(text, emoji):
    """Back from a leave"""
    config.slack.client.users.set_presence("auto")
    ctx = click.get_current_context()
    ctx.forward(_set)


@slack.group()
def pomodoro_status():
    """Handle the status to show the pomodoro state"""
    # presence = config.slack.client.users.get_presence(
    #     config.slack.me["id"]
    # ).body["presence"]
    # if presence == "away":
    #     LOGGER.info("Not updating the status if your are away")
    #     exit(0)


@pomodoro_status.command()
@argument("end", help="The end of the pomodoro", type=dateparse)
def start(end):
    """Indicate you are in a pomodoro that will spend till end"""
    ctx = click.get_current_context()
    ctx.invoke(
        _set,
        text="In a pomodoro, hopefully till {}".format(
            end.strftime("%H:%M")
        ),
        emoji="tomato"
    )


@pomodoro_status.command()
@argument("end", help="The end of the pomodoro", type=dateparse)
def _break(end):
    """Indicate you are in a break that will spend till end"""
    ctx = click.get_current_context()
    ctx.invoke(
        _set,
        text="In a break, hopefully till {}".format(
            end.strftime("%H:%M")
        ),
        emoji="zzz"
    )


@pomodoro_status.command()
def _stop():
    """Reset the status"""
    ctx = click.get_current_context()
    ctx.invoke(
        _set,
        text="",
        emoji="",
    )


@slack.group()
def dnd():
    """Handle the Do Not Disturb status"""


@dnd.command()
@argument("user", type=UserType(), required=False,
          help="the user to take into account, defaulting to myself.")
def info(user):
    """Show the dnd status of the user"""
    user = user or config.slack.me_user
    resp = config.slack.client.dnd.info(user.id).body
    print("Snoozing: {}".format("yes" if resp["snooze_enabled"] else "no"))
    if resp["snooze_enabled"]:
        print("From {}".format(
            datetime.datetime.fromtimestamp(resp["next_dnd_start_ts"])))
        print("To   {}".format(
            datetime.datetime.fromtimestamp(resp["next_dnd_end_ts"])))


@dnd.command()
def end():
    """End the dnd session"""
    config.slack.client.dnd.end_dnd()


@slack.command()
@argument("enddate",
          help="The end of the snooze, if prefixed with +, the number of minutes to snooze",
          default="+30")
@option("--text", help="The text of the status")
@option("--emoji", help="The emoji of the status")
def snooze(enddate, text, emoji):
    """Snooze notifications by that amount of minutes"""
    if enddate.startswith("+"):
        minutes = int(enddate[1:])
    else:
        cal = parsedatetime.Calendar()
        enddate = cal.parseDT(enddate)[0]
        minutes = (enddate - datetime.datetime.now()).total_seconds() / 60
    resp = config.slack.client.dnd.set_snooze(minutes).body
    endtime = datetime.datetime.fromtimestamp(resp["snooze_endtime"])
    LOGGER.info("Snooze will end at {}".format(endtime))
    if text is not None or emoji is not None:
        ctx = click.get_current_context()
        ctx.invoke(_set, text=text, emoji=emoji)


@slack.command()
def end_snooze():
    """End the snooze"""
    config.slack.client.dnd.end_snooze()
