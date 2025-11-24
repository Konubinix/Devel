"""My custom conf of qutebrowser."""

import os
from pathlib import Path

# pylint: disable=C0111
c = c  # noqa: F821 pylint: disable=E0602,C0103
config = config  # noqa: F821 pylint: disable=E0602,C0103

# #+name: 32887b11-21da-4230-bc3a-d10ddc67a0a6
# #+BEGIN_QUOTE
# When a config.py exists, the autoconfig.yml file is not read anymore by
# default. You need to load it from config.py

# --- qute://help/configuring.html#autoconfig ([2025-10-29])
# #+END_QUOTE
config.load_autoconfig()

config.set(
    "content.user_stylesheets",
    [
        str(Path(os.environ["KONIX_PERSO_CONFIG_DIR"]) / "qutebrowser/konubinix.css"),
    ],
)
