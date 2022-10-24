#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import base64
import datetime
import email
import mailbox
import re
import uuid
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from hashlib import md5

import html2text
import six
from bs4 import BeautifulSoup as bs


def format_markdown(from_, to, subject, date, content):
    import markdown
    m = markdown.Markdown()
    html_content = m.convert(content)
    content_lines = content.splitlines()

    msg = MIMEMultipart('alternative')
    msg['Subject'] = subject
    msg['From'] = from_
    msg['To'] = to
    if date:
        if isinstance(date, six.string_types):
            msg['Date'] = date
        else:
            msg["Date"] = email.utils.formatdate((int(date.strftime("%s"))),
                                                 date)
    plain_part = MIMEText(content, _subtype='plain', _charset="utf-8")
    html_part = MIMEText(html_content, _subtype='html', _charset="utf-8")
    msg.attach(plain_part)
    msg.attach(html_part)
    return msg


def format_mail(from_, to, subject, date, pure_text_prefix, content):
    content_lines = content.splitlines()

    try:
        text_content = pure_text_prefix + u"\n\n" + html2text.html2text(
            content)
    except:
        text_content = "No able to convert to text, sorry. Read the html version instead"
    html_content = pure_text_prefix + u"\n\n" + content

    text_content = text_content.encode("utf-8")
    html_content = html_content.encode("utf-8")

    msg = MIMEMultipart('alternative')
    msg['Subject'] = subject
    msg['From'] = from_
    msg['To'] = to
    if date:
        if isinstance(date, six.string_types):
            msg['Date'] = date
        else:
            msg["Date"] = email.utils.formatdate((int(date.strftime("%s"))),
                                                 date)
    plain_part = MIMEText(text_content, _subtype='plain', _charset="utf-8")
    html_part = MIMEText(html_content, _subtype='html', _charset="utf-8")
    msg.attach(plain_part)
    msg.attach(html_part)
    if six.PY3:
        msg_bytes = msg.as_bytes()
    else:
        msg_bytes = msg.as_string()
    msg["Message-Id"] = "<{}>".format(md5(msg_bytes).hexdigest())
    return msg


add_to_maildir_hooks = []


def add_to_maildir(from_, to, subject, date, pure_text_prefix, content,
                   directory):
    msg = format_mail(from_, to, subject, date, pure_text_prefix, content)
    box = mailbox.Maildir(directory)
    for hook in add_to_maildir_hooks:
        msg = hook(msg)
    return box.add(str(msg))


def use_relative_links(html, directory="."):
    from pathlib import Path
    return html.replace(f"file://{Path(directory).absolute()}/", "./")


def make_part_harmless(html):
    s = bs(html, "lxml")
    for img in s.find_all("img", src=True):
        if img.attrs["src"].startswith("data:"):
            continue
        if img.attrs["src"].startswith("file:"):
            continue
        src = img.attrs.pop("src")
        id = img.attrs.get("id", str(uuid.uuid1()))
        img.attrs["id"] = id
        img.attrs["datasrc"] = src
        img.attrs["src"] = "http://a.a"
        img.attrs["oldstyle"] = img.attrs.get("style", "")
        img.attrs["style"] = "border: solid 3px blue;" + img.attrs["oldstyle"]
        alt = img.attrs.get("alt")
        if alt is None:
            alt = f"CLICKTOLOAD ({src})"
        elif alt == "":
            alt = "EmptyAlt CLICKTOLOAD"
        img.attrs["alt"] = alt
        img.attrs[
            "onmousedown"] = 'this.style = this.getAttribute("oldstyle") ; this.src = this.getAttribute("datasrc");'
        img.attrs["ontouchstart"] = img.attrs["onmousedown"]
    for elem in s.find_all("meta", attrs={"name": "viewport"}):
        elem.extract()
    for elem in s.find_all("link", attrs={"type": "text/css"}, src=True):
        elem.extract()
    return str(s)


def html_inject_cid(html, msg):
    soup = bs(html, "lxml")
    for img in soup.find_all(src=re.compile("cid:.+")):
        cid = img.attrs["src"][4:]
        for part in msg.walk():
            if part["content-id"] == "<{}>".format(cid):
                img.attrs["src"] = "data:{};base64,{}".format(
                    part.get_content_type(),
                    base64.encodestring(
                        part.get_payload(decode=True)).decode("utf-8").strip(),
                )
                break
    return str(soup)
