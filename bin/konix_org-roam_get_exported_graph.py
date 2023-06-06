#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import json
import os
import sqlite3
import sys
from pathlib import Path


def main(db_location, exported_path, kind):
    roam = sqlite3.connect(db_location)
    exported = [
        f'"{file}"' for file in exported_path.read_text().strip().splitlines()
    ]
    links = []
    handled = set()
    nodes = []

    def name(node):
        return node.replace('"', "").replace(
            str(Path(f"{os.environ['KONIX_PERSO_DIR']}/roam/").resolve()),
            "",
        )[: -len(".org")]

    def add_node(node, file):
        if node not in handled:
            handled.add(node)
            nodes.append(
                {"id": node, "url": f"/{kind}/posts{name(file)}/", "group": 1}
            )

    for src_title, dest_title, src, dest in roam.execute(
        "select sources.title, destinations.title, sources.file, destinations.file from links inner join nodes as sources on sources.id = links.source inner join nodes as destinations on destinations.id = links.dest"
    ):
        src_title = src_title.replace('"', "")
        dest_title = dest_title.replace('"', "")
        if src in exported and dest in exported:
            add_node(src_title, src)
            add_node(dest_title, dest)
            links.append(
                {
                    "source": src_title,
                    "target": dest_title,
                    "type": "file",
                }
            )
    print(
        json.dumps(
            {
                "links": links,
                "nodes": nodes,
            }
        )
    )


if __name__ == "__main__":
    main(
        Path(sys.argv[1]),
        Path(sys.argv[2]),
        sys.argv[3],
    )
