#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from clk.decorators import group, argument


@group()
def pulse():
    """Manipulate the sound"""


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
