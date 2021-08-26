#! /usr/bin/env python3

from pyrillib import main, configs
configs.get("auto_select").activated = True
configs.get("auto_open").activated = True
configs.get("recompute").activated = False
main()
