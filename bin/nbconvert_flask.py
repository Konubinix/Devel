#!/usr/bin/env python
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
INPUT = sys.argv[1]
OUTPUT = sys.argv[2]
logging.debug("INPUT :{}".format(INPUT))
logging.debug("OUTPUT:{}".format(OUTPUT))
if not os.path.exists(OUTPUT):
    os.makedirs(OUTPUT)
os.chdir(OUTPUT)

def nbconvert(input_file_):
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
    os.system("nbconvert_html " + input_filename)

app = Flask(__name__)

@app.route('/get/<path:filename>')
def serve_file(filename):
    logging.debug("Serving: {}".format(filename))
    if filename == "custom.css":
        return send_file(
            CUSTOM_CSS_PATH
        )
    else:
        return serve_ipynb(filename)

def serve_ipynb(filename):
    input_filename = "{}.{}".format(
        os.path.splitext(filename)[0],
        "ipynb"
    )
    nbconvert(input_filename)
    return send_file(
        os.path.join(OUTPUT, filename)
    )

if __name__ == "__main__":
    app.run(host='0.0.0.0', port=9638, debug=True)
