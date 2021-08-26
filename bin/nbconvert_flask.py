#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from flask import request, Flask, send_file
import os
import sys
import logging
logging.basicConfig(level=logging.DEBUG)

CUSTOM_CSS_PATH=os.path.join(
    os.environ["JUPYTER_CONFIG_DIR"],
    "custom",
    "custom.css"
)
import argparse
parser = argparse.ArgumentParser(description="""Nb viewer.""")

parser.add_argument('-i','--input-notebooks',
                    help="""Directory with notebooks inside""",
                    type=str,
                    default=os.environ["KONIX_NOTEBOOKS_DIR"])

parser.add_argument('-o','--output-html',
                    help="""Directory with rendered html files""",
                    type=str,
                    default=os.environ["KONIX_NBVIEWER_HTML_DIR"])

parser.add_argument('-p','--port',
                    help="""No comment""",
                    type=int,
                    default=int(os.environ["KONIX_NBVIEWER_PORT"])
)

parser.add_argument('--ip',
                    help="""No comment""",
                    type=str,
                    default=os.environ["KONIX_NBVIEWER_IP"]
)

args = parser.parse_args()

INPUT = args.input_notebooks
OUTPUT = args.output_html
logging.debug("INPUT :{}".format(INPUT))
logging.debug("OUTPUT:{}".format(OUTPUT))
if not os.path.exists(OUTPUT):
    os.makedirs(OUTPUT)
os.chdir(OUTPUT)

def nbconvert(input_file_, execute=False, show_input=False):
    input_filename = os.path.join(
        INPUT,
        input_file_
    )
    os.chdir(OUTPUT)
    dir_ = os.path.dirname(input_file_)
    if dir_:
        if not os.path.exists(dir_):
            os.makedirs(dir_)
        os.chdir(dir_)
    input_basename = os.path.basename(input_filename)
    if show_input:
        suffix = "_input"
    else:
        suffix = ""
    output_basename = os.path.splitext(input_basename)[0] + suffix + ".html"
    output_filename = os.path.join(dir_, output_basename)
    if (
            not execute
            and
            os.path.exists(output_basename)
            and
            os.stat(output_basename).st_mtime > os.stat(input_filename).st_mtime
    ):
        return output_filename
    else:
        os.system(
            "nbconvert_html --output={} {} --execute '{}'".format(
                output_basename,
                "" if show_input else "--template=hide_input.tpl",
                input_filename,
            )
        )
    return output_filename

app = Flask(__name__)

@app.route('/get/<path:filename>')
def serve_file(filename):
    execute = bool(request.values.get("execute"))
    show_input = bool(request.values.get("input"))
    logging.debug("Serving: {}".format(filename))

    if filename.endswith("custom.css"):
        return send_file(
            CUSTOM_CSS_PATH
        )
    else:
        return serve_ipynb(filename, execute=execute, show_input=show_input)

def serve_ipynb(filename, execute=False, show_input=False):
    input_filename = "{}.{}".format(
        os.path.splitext(filename)[0],
        "ipynb"
    )
    output_filename = nbconvert(input_filename, execute=execute, show_input=show_input)
    return send_file(
        os.path.join(OUTPUT, output_filename)
    )

if __name__ == "__main__":
    app.run(host=args.ip, port=args.port, debug=True)
