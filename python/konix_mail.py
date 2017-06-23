#!/usr/bin/env python
# -*- coding:utf-8 -*-

from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from bs4 import BeautifulSoup as bs
import uuid
import email
import datetime
import mailbox
import html2text


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
        if isinstance(date, basestring):
            msg['Date'] = date
        else:
            msg["Date"] = email.utils.formatdate((int(date.strftime("%s"))), date)
    plain_part = MIMEText(content, _subtype='plain', _charset="utf-8")
    html_part = MIMEText(html_content, _subtype='html', _charset="utf-8")
    msg.attach(plain_part)
    msg.attach(html_part)
    return msg


def format_mail(from_, to, subject, date, pure_text_prefix, content):
    content_lines = content.splitlines()

    text_content = pure_text_prefix + u"\n\n" + html2text.html2text(content)
    html_content = pure_text_prefix + u"\n\n" + content

    text_content = text_content.encode("utf-8")
    html_content = html_content.encode("utf-8")

    msg = MIMEMultipart('alternative')
    msg['Subject'] = subject
    msg['From'] = from_
    msg['To'] = to
    if date:
        if isinstance(date, basestring):
            msg['Date'] = date
        else:
            msg["Date"] = email.utils.formatdate((int(date.strftime("%s"))), date)
    plain_part = MIMEText(text_content, _subtype='plain', _charset="utf-8")
    html_part = MIMEText(html_content, _subtype='html', _charset="utf-8")
    msg.attach(plain_part)
    msg.attach(html_part)
    return msg


add_to_maildir_hooks = []


def add_to_maildir(from_, to, subject, date, pure_text_prefix, content, directory):
    msg = format_mail(from_, to, subject, date, pure_text_prefix, content)
    box = mailbox.Maildir(directory)
    for hook in add_to_maildir_hooks:
        msg = hook(msg)
    return box.add(str(msg))


def make_part_harmless(html):
    s = bs(html, "lxml")
    for img in s.find_all("img", src=True):
        src = img.attrs.pop("src")
        id = img.attrs.get("id", str(uuid.uuid1()))
        img.attrs["id"] = id
        img.attrs["datasrc"] = src
        img.attrs["src"] = "http://a.a"
        img.attrs["alt"] = img.attrs.get("alt", "Load me")
        img.attrs["onmousedown"] = 'this.src = this.getAttribute("datasrc");'
        img.attrs["ontouchstart"] = img.attrs["onmousedown"]
    return str(s)
