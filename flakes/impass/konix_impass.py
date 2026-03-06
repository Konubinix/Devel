#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from subprocess import check_output

from impass import gui

_original_return_value = gui.Gui.return_value


def _patched_return_value(self):
    breakpoint()
    result = _original_return_value(self)
    if result:
        # Use entry text if GUI was shown, otherwise fall back to query
        key = None
        if hasattr(self, 'entry') and self.entry is not None:
            key = self.entry.get_text().strip()
        elif hasattr(self, 'query') and self.query:
            key = self.query.strip()
        if key and key.endswith("_totp"):
            result["password"] = check_output(
                ["clk", "otp", key[:-len("_totp")]]).strip()
    return result


gui.Gui.return_value = _patched_return_value

from impass.__main__ import main

if __name__ == "__main__":
    main()
