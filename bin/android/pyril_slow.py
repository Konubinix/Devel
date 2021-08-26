#! /usr/bin/env python3

from pyrillib import main, configs, conditions
configs.get("auto_select").activated = False
configs.get("auto_open").activated = False
configs.get("recompute").activated = False
conditions.get("dled").activated = False
conditions.get("unread").activated = False
main()
