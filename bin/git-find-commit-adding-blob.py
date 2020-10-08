#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
from multiprocessing import Pool, Manager
import shlex
import sys
import re
import builtins
from collections import defaultdict

from joblib import Parallel, delayed, Memory
import click
from click_project.lib import check_output, progressbar
from walrus import Database


cache = Database(host='localhost').cache(name=os.path.basename(__file__), default_timeout=24 * 3600)
cache2 = Memory("/home/sam/tmp/{}".format(os.path.basename(__file__)), verbose=0)
ls_tree_re = re.compile("^[^ ]+ [^ ]+ (?P<object>[^\t]+)\t(?P<path>.+)$")


def empty(l):
    try:
        next(iter(l))
        return False
    except StopIteration:
        return True


@cache2.cache
def commit_blobs(commit_sha):
    return check_output(
        shlex.split("git ls-tree -t -r {}".format(commit_sha))
    ).splitlines()


@cache.cached()
def commit_matching_path(commit_sha, blob_sha):
    blobs_matches = (
         ls_tree_re.match(line)
         for line in commit_blobs(commit_sha)
     )
    for match in blobs_matches:
        if match.group("object").startswith(blob_sha):
            return match.group("path")
    return None


@cache.cached()
def matching(commit, parents, blob_sha):
    res = commit_matching_path(commit, blob_sha)
    # print("trying {}".format(commit))
    if res and not builtins.any(
            commit_matching_path(parent, blob_sha) for parent in parents
    ):
        return res
    else:
        return None


def find_matching(info):
    commit, parents, blob_sha, end = info
    if end.get():
        return
    res = matching(commit, parents, blob_sha)
    if res:
        end.set(True)
    return res


@cache.cached()
def pool(blob_sha):
    lines = [
        line.split(" ")
        for line in check_output(
                shlex.split("git rev-list --parents HEAD")
        ).splitlines()
    ]
    p = Pool()
    manager = Manager()
    end = manager.Value(bool, False)
    gen = p.imap(find_matching, [
        (commit, parents, blob_sha, end)
        for commit, *parents in lines  # NOQA
    ])
    with progressbar(gen, length=len(lines)) as bar:
        for line, res in zip(lines, bar):
            if res:
                p.terminate()
                p.join()
                return line, res


@click.command()
@click.argument("blob-sha")
def main(blob_sha):
    def attempt1():
        info = pool(blob_sha)
        if info:
            line, res = info
            commit, *parents = line
            print("## {}".format(blob_sha))
            print(
                check_output(
                    shlex.split(
                        "git log --format='%cd %h %s' -n 1 {}".format(commit))
                )
            )
            print("  {}".format(res))
            print("-------------")

    @cache2.cache
    def big_dict():
        lines = [
            line.split(" ")
            for line in check_output(
                    shlex.split("git rev-list --parents HEAD")
            ).splitlines()
        ]
        blob_to_commit = defaultdict(set)
        parenthood = {}
        with progressbar(lines) as bar:
            for commit, *parents in bar:
                for blob in commit_blobs(commit):
                    blob_to_commit[blob].add(commit)
                for parent in parents:
                    for blob in commit_blobs(parent):
                        blob_to_commit[blob].add(parent)
                parenthood[commit] = parents
        return blob_to_commit, parenthood
    attempt1()


if __name__ == "__main__":
    main()
