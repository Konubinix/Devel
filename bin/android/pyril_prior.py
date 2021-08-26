#! /usr/bin/env python3

from pyrillib import main, configs, conditions
configs.get("auto_select").activated = True
configs.get("auto_open").activated = False
configs.get("recompute").activated = False
configs.get("prior_back").activated = True
conditions.get("dled").activated = False
conditions.get("unread").activated = False
conditions.get("prior impl").activated = True
main()
