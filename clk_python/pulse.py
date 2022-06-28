#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import subprocess
from shlex import split

from clk import call, check_output
from clk.decorators import argument, flag, group
from clk.types import DynamicChoice


@group()
def pulse():
    """Manipulate the sound"""


class PlayingApplicationsTypes(DynamicChoice):

    def choices(self):
        import pulsectl
        with pulsectl.Pulse("type") as p:
            return [
                f'{sink_input.proplist["application.process.binary"]}-{sink_input.client}'
                for sink_input in p.sink_input_list()
                if "application.process.binary" in sink_input.proplist
            ]

    def converter(self, value):
        binary, client = value.split("-")
        import pulsectl
        with pulsectl.Pulse("type") as p:
            return [
                sink_input for sink_input in p.sink_input_list()
                if sink_input.proplist.get("application.process.binary") ==
                binary and str(sink_input.client) == client
            ][0]


def find_sink(index):
    import pulsectl
    with pulsectl.Pulse("clk") as p:
        return [sink for sink in p.sink_list() if sink.index == index][0]


def find_sink_by_name(name):
    import pulsectl
    with pulsectl.Pulse("clk") as p:
        return [sink for sink in p.sink_list() if sink.name == name][0]


def find_sink_by_name_safe(name):
    try:
        return find_sink_by_name(name)
    except IndexError:
        return None


@pulse.command()
@argument("application",
          help="What application to record",
          type=PlayingApplicationsTypes())
@argument("output", help="Where to put the record")
@flag("--mute", help="Record the stream while muting the source")
def record(application, output, mute):
    """Record the application sound"""
    import pulsectl
    sink = find_sink(application.sink)
    proxy_name = f"proxy-{application.proplist['application.process.binary']}"
    proxy_sink = find_sink_by_name_safe(proxy_name)
    if proxy_sink is None:
        with pulsectl.Pulse("clk") as p:
            if mute:
                proxy_id = p.module_load("module-null-sink",
                                         f"sink_name={proxy_name}")
            else:
                proxy_id = p.module_load(
                    "module-combine-sink",
                    f"sink_name={proxy_name} slaves={sink.name}")

        from clk.atexit import register

        def clean_sink(proxy_id):
            with pulsectl.Pulse("clk") as p:
                p.module_unload(proxy_id)

        register(clean_sink, proxy_id=proxy_id)

        proxy_sink = find_sink_by_name(proxy_name)
    with pulsectl.Pulse("clk") as p:
        proxy_id = p.sink_input_move(application.index, proxy_sink.index)
    with open(output, 'w') as f:
        subprocess.run(["parecord", "-d", f"{proxy_sink.monitor_source_name}"],
                       stdout=f)


@pulse.command()
@argument("value", type=float, default=0.1, help="The value to use")
def up(value):
    """Increase the volume"""
    import pulsectl
    with pulsectl.Pulse('volume-increaser') as pulse:
        for sink in pulse.sink_list():
            pulse.volume_change_all_chans(sink, value)


@pulse.command()
@argument("value", type=float, default=0.1, help="The value to use")
def down(value):
    """Decrease the volume"""
    import pulsectl
    with pulsectl.Pulse('volume-decreaser') as pulse:
        for sink in pulse.sink_list():
            pulse.volume_change_all_chans(sink, -value)


@pulse.command()
def show():
    """Show the volume"""
    import pulsectl
    with pulsectl.Pulse('volume-increaser') as pulse:
        for sink in pulse.sink_list():
            if sink.mute:
                print(f"{sink.description} muted")
            print(
                f"{sink.description}:\n\t{sink.volume.values[0]:.2f}, {sink.volume.values[1]:.2f}"
            )


@pulse.command()
def toggle_mute():
    """Toggle mute"""
    import pulsectl
    with pulsectl.Pulse('volume-mute-toggler') as pulse:
        for sink in pulse.sink_list():
            pulse.mute(sink, not sink.mute)
