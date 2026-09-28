#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import base64
import datetime
import email
import mailbox
import os
import re
import uuid
from email.header import decode_header
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

    msg = MIMEMultipart("alternative")
    msg["Subject"] = subject
    msg["From"] = from_
    msg["To"] = to
    if date:
        if isinstance(date, six.string_types):
            msg["Date"] = date
        else:
            msg["Date"] = email.utils.formatdate((int(date.strftime("%s"))), date)
    plain_part = MIMEText(content, _subtype="plain", _charset="utf-8")
    html_part = MIMEText(html_content, _subtype="html", _charset="utf-8")
    msg.attach(plain_part)
    msg.attach(html_part)
    return msg


def format_mail(from_, to, subject, date, pure_text_prefix, content):
    content_lines = content.splitlines()

    try:
        text_content = pure_text_prefix + "\n\n" + html2text.html2text(content)
    except:
        text_content = (
            "No able to convert to text, sorry. Read the html version instead"
        )
    html_content = pure_text_prefix + "\n\n" + content

    text_content = text_content.encode("utf-8")
    html_content = html_content.encode("utf-8")

    msg = MIMEMultipart("alternative")
    msg["Subject"] = subject
    msg["From"] = from_
    msg["To"] = to
    if date:
        if isinstance(date, six.string_types):
            msg["Date"] = date
        else:
            msg["Date"] = email.utils.formatdate((int(date.strftime("%s"))), date)
    plain_part = MIMEText(text_content, _subtype="plain", _charset="utf-8")
    html_part = MIMEText(html_content, _subtype="html", _charset="utf-8")
    msg.attach(plain_part)
    msg.attach(html_part)
    if six.PY3:
        msg_bytes = msg.as_bytes()
    else:
        msg_bytes = msg.as_string()
    msg["Message-Id"] = "<{}>".format(md5(msg_bytes).hexdigest())
    return msg


add_to_maildir_hooks = []


def add_to_maildir(from_, to, subject, date, pure_text_prefix, content, directory):
    msg = format_mail(from_, to, subject, date, pure_text_prefix, content)
    box = mailbox.Maildir(directory)
    for hook in add_to_maildir_hooks:
        msg = hook(msg)
    return box.add(str(msg))


def use_relative_links(html, directory="."):
    from pathlib import Path

    return html.replace(f"file://{Path(directory).absolute()}/", "./")


def make_part_harmless(html):
    harmless_prefixes = os.getenv(
        "KONIX_MAIL_HARMLESS_IMAGE_PREFIXES",
        "",
    ).split(",")

    s = bs(html, "html.parser")
    for img in s.find_all("img", src=True):
        if any(img.attrs["src"].startswith(prefix) for prefix in harmless_prefixes):
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
        img.attrs["onmousedown"] = (
            'this.style = this.getAttribute("oldstyle") ; this.src = this.getAttribute("datasrc");'
        )
        img.attrs["ontouchstart"] = img.attrs["onmousedown"]
    for elem in s.find_all("meta", attrs={"name": "viewport"}):
        elem.extract()
    for elem in s.find_all("link", attrs={"type": "text/css"}, src=True):
        elem.extract()
    return str(s)


XHTML = """<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
<title>Mail</title>
<meta  http-equiv="Content-Type" content="text/html;charset=utf-8" />
</head>
<body>
{}
</body>
</html>
"""


def parse_mail(raw):
    """Parse the raw bytes of a mail, falling back to latin-1 when it is not
    utf-8"""
    for encoding in ("utf-8", "latin-1"):
        try:
            return email.message_from_string(raw.decode(encoding))
        except UnicodeDecodeError:
            pass


def decode_header_value(value):
    return ", ".join(
        part if isinstance(part, str) else part.decode(encoding or "utf-8")
        for part, encoding in decode_header(value)
    )


def munpack(mail, directory):
    """Write the parts of mail into directory, pointing the cid: links to the
    files of the parts they refer to"""
    from hashlib import sha256
    from pathlib import Path

    from konix_file_helper import sanitize_filename

    parts = [part for part in mail.walk() if part.get_payload(decode=True) is not None]
    subject = sanitize_filename(decode_header_value(mail["Subject"]))
    lone_subtypes = {
        subtype
        for subtype in ("html", "plain")
        if len([part for part in mail.walk() if part.get_content_subtype() == subtype]) == 1
    }

    def name(part):
        if filename := part.get_filename():
            return decode_header_value(filename)
        subtype = part.get_content_subtype()
        if subtype in lone_subtypes:
            return f"{subject}.{subtype}"
        suffix = sha256(part.get_payload(decode=True)).hexdigest()[:10]
        return f"{part.get_content_maintype()}-{suffix}.{subtype}"

    cid_links = {
        f"cid:{part['Content-Id'].strip('<>')}": f"./{name(part)}"
        for part in parts
        if part["Content-Id"]
    }
    for part in parts:
        content = part.get_payload(decode=True)
        path = Path(directory) / name(part)
        if part.get_content_maintype() == "text":
            charset = part.get_content_charset() or "utf-8"
            if "ascii" in charset:
                charset = "utf-8"
            try:
                text = content.decode(charset)
            except UnicodeDecodeError:
                text = part.get_payload()
            for cid, link in cid_links.items():
                text = text.replace(cid, link)
            content = text.encode(charset)
            if part.get_content_subtype() == "html":
                Path(f"{path}_orig").write_text(text)
                content = XHTML.format(make_part_harmless(text)).encode("utf-8")
        path.write_bytes(content)


def html_inject_cid(html, msg):
    soup = bs(html, "lxml")
    for img in soup.find_all(src=re.compile("cid:.+")):
        cid = img.attrs["src"][4:]
        for part in msg.walk():
            if part["content-id"] == "<{}>".format(cid):
                img.attrs["src"] = "data:{};base64,{}".format(
                    part.get_content_type(),
                    base64.encodestring(part.get_payload(decode=True))
                    .decode("utf-8")
                    .strip(),
                )
                break
    return str(soup)
