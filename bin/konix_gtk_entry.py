#!/usr/bin/env python3
"""A poor man clone of zenity --entry, using dmenu and notify-send."""

import argparse
import subprocess
import sys

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("-t", "--text", help="The text to display", type=str, required=True)
parser.add_argument(
    "-T", "--timeout", help="Close after that amount of ms", type=int, default=0
)
parser.add_argument("-i", "--initial", help="The initial text", type=str)
parser.add_argument("--info", help="Do not ask any input", action="store_true")
parser.add_argument(
    "-n", "--no-clipboard", help="Disable clipboard handling", action="store_true"
)
parser.add_argument(
    "-a", "--above-all", help="Ignored (dmenu is already on top)", action="store_true"
)
parser.add_argument(
    "-b", "--background-color", help="Ignored", type=str
)


def get_clipboard():
    for sel in ("primary", "clipboard"):
        try:
            return subprocess.check_output(
                ["xclip", "-selection", sel, "-o"], stderr=subprocess.DEVNULL
            ).decode().strip()
        except (subprocess.CalledProcessError, FileNotFoundError):
            continue
    return ""


def main():
    args = parser.parse_args()

    if args.info:
        cmd = ["notify-send"]
        if args.timeout > 0:
            cmd += ["-t", str(args.timeout)]
        cmd += ["konix_gtk_entry", args.text]
        subprocess.run(cmd)
        return

    if args.initial:
        input_text = args.initial
    elif not args.no_clipboard:
        input_text = get_clipboard()
    else:
        input_text = ""

    dmenu_cmd = ["dmenu", "-p", args.text]

    if args.timeout > 0:
        timeout_s = (args.timeout + 999) // 1000
        dmenu_cmd = ["timeout", str(timeout_s)] + dmenu_cmd

    try:
        result = subprocess.run(
            dmenu_cmd,
            input=input_text + "\n",
            capture_output=True,
            text=True,
        )
        print(result.stdout.strip())
    except Exception:
        sys.exit(1)


if __name__ == "__main__":
    main()
