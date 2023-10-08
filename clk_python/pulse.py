#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import subprocess
from shlex import split

import click
from clk import call, check_output, run
from clk.config import config
from clk.decorators import argument, flag, group, option
from clk.log import get_logger
from clk.types import DynamicChoice

LOGGER = get_logger(__name__)


class Pulse:
    pass


class Applications:

    @property
    def sinks_input(self):
        return [
            application for application in self.applications
            if application.__class__.__name__ == "PulseSinkInputInfo"
        ]

    @property
    def sources_output(self):
        return [
            application for application in self.applications
            if application.__class__.__name__ == "PulseSourceOutputInfo"
        ]


@group()
def pulse():
    """Manipulate the sound"""
    config.pulse = Pulse()


def find_sink(index):
    import pulsectl
    with pulsectl.Pulse("clk") as p:
        return [sink for sink in p.sink_list() if sink.index == index][0]


class PlayingApplicationsTypes(DynamicChoice):

    def choices(self):
        import pulsectl
        with pulsectl.Pulse("type") as p:
            return [
                f'{sink_input.proplist["application.process.binary"]}/{find_sink(sink_input.sink).name}/{sink_input.client}'
                for sink_input in p.sink_input_list()
                if "application.process.binary" in sink_input.proplist
            ]

    def convert(self, value, param, ctx):
        if "/" in value:
            binary, _, client = value.split("/")
        else:
            binary = value
            client = None
        import pulsectl
        with pulsectl.Pulse("type") as p:
            return [
                sink_input for sink_input in p.sink_input_list()
                if sink_input.proplist.get("application.process.binary") ==
                binary and (not client or str(sink_input.client) == client)
            ][0]


sink_aliases = {
    "headset": "casque",
    "casque":
    "alsa_output.usb-C-Media_Electronics_Inc._USB_Audio_Device-00.analog-stereo",
    "speaker": "alsa_output.usb-GeneralPlus_USB_Audio_Device-00.analog-stereo",
    "laptop": "alsa_output.pci-0000_00_1f.3.analog-stereo",
}

source_aliases = {
    "casque":
    "alsa_input.usb-C-Media_Electronics_Inc._USB_Audio_Device-00.mono-fallback",
}


class SinkType(DynamicChoice):

    def choices(self):
        import pulsectl
        with pulsectl.Pulse("type") as p:
            return [sink.name
                    for sink in p.sink_list()] + list(sink_aliases.keys())

    def converter(self, value):
        import pulsectl
        while value in sink_aliases:
            value = sink_aliases[value]
        with pulsectl.Pulse("type") as p:
            return [sink for sink in p.sink_list() if sink.name == value][0]


class SourceType(DynamicChoice):

    def choices(self):
        import pulsectl
        with pulsectl.Pulse("type") as p:
            return [source.name for source in p.source_list()] + list(
                source_aliases.keys())

    def converter(self, value):
        import pulsectl
        while value in source_aliases:
            value = source_aliases[value]
        with pulsectl.Pulse("type") as p:
            return [
                source for source in p.source_list() if source.name == value
            ][0]


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
@argument(
    "sink",
    help="What sink to use instead",
    type=SinkType(),
)
@argument(
    "application",
    help="What application to switch",
    type=PlayingApplicationsTypes(),
)
def change_sink(application, sink):
    """Move the application to another sink"""
    import pulsectl
    with pulsectl.Pulse("clk") as p:
        p.sink_input_move(application.index, sink.index)


@pulse.group(default_command="show")
@flag("--all", help="All the applications")
@option("--name-prefix", help="With name starting with")
def application(all, name_prefix):
    "Manage applications"
    config.pulse.applications = Applications()
    import pulsectl
    applications = []
    with pulsectl.Pulse("clk") as p:

        def all_applications():
            return [
                application for application in p.sink_input_list() +
                p.source_output_list() if application.proplist.get(
                    "application.process.binary") != "pavucontrol"
            ]

        if all:
            applications = all_applications()
        elif name_prefix:
            applications = [
                application for application in all_applications()
                if application.proplist.get("application.process.binary", "").
                lower().startswith(name_prefix.lower())
            ]

    config.pulse.applications.applications = applications


@application.command()
def show():
    "Show the matching applications"
    for application in config.pulse.applications.applications:
        print(
            f'{application.__class__.__name__} : {application.proplist.get("application.process.binary")} : {application}'
        )


@application.command()
@argument(
    "sink",
    help="What sink to use instead",
    type=SinkType(),
)
def move_to_sink(sink):
    "Move the matching application to this sink"
    import pulsectl
    with pulsectl.Pulse("clk") as p:
        for application in config.pulse.applications.sinks_input:
            p.sink_input_move(application.index, sink.index)


@application.command()
@argument(
    "source",
    help="What source to use instead",
    type=SourceType(),
)
def move_to_source(source):
    "Move the matching application to this source"
    import pulsectl
    with pulsectl.Pulse("clk") as p:
        for application in config.pulse.applications.sources_output:
            p.source_output_move(application.index, source.index)


@pulse.command()
@argument("application",
          help="What application to record",
          type=PlayingApplicationsTypes())
@option("--output", help="Where to put the record", default="output.ogg")
@flag("--mute", help="Record the stream while muting the source")
def record(application, output, mute):
    """Record the application sound"""
    import pulsectl
    sink = find_sink(application.sink)
    proxy_name = f"proxy-{application.proplist['application.process.binary']}-{application.client}-{sink.name}"
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
                try:
                    p.sink_input_move(application.index, sink.index)
                except pulsectl.PulseOperationFailed:
                    LOGGER.info("Could not restore the application sink,"
                                " most likely the application was stopped"
                                " in the mean time."
                                " So, no big deal.")
                p.module_unload(proxy_id)

        register(clean_sink, proxy_id=proxy_id)

        proxy_sink = find_sink_by_name(proxy_name)
    with pulsectl.Pulse("clk") as p:
        proxy_id = p.sink_input_move(application.index, proxy_sink.index)
    run([
        "pulse",
        "record-sink",
        f"{proxy_sink.monitor_source_name}",
        "--output",
        output,
    ], )


@pulse.group()
@option("--sink", type=SinkType(), multiple=True, help="The sink to setup")
def sink(sink):
    """Manipulate the sinks"""
    import pulsectl
    with pulsectl.Pulse('volume-setter') as pulse:
        config.sink = sink or pulse.sink_list()


@sink.command()
def set_as_default():
    """Set this sink is the default one"""
    if len(config.sink) > 1:
        raise click.UsageError("You must select at most one sink")
    import pulsectl
    with pulsectl.Pulse() as pulse:
        pulse.default_set(config.sink[0])


@sink.command()
@argument("value", type=float, help="The value to use")
def _set(sink, value):
    """Put the volume"""
    import pulsectl
    with pulsectl.Pulse('volume-setter') as pulse:
        for sink in config.sink:
            pulse.volume_set_all_chans(sink, value)


@sink.command()
@argument("value", type=float, default=0.1, help="The value to use")
def up(value):
    """Increase the volume"""
    import pulsectl
    with pulsectl.Pulse('volume-increaser') as pulse:
        for sink in config.sink:
            pulse.volume_change_all_chans(sink, value)


@sink.command()
@argument("value", type=float, default=0.1, help="The value to use")
def down(value):
    """Decrease the volume"""
    import pulsectl
    with pulsectl.Pulse('volume-decreaser') as pulse:
        for sink in config.sink:
            pulse.volume_change_all_chans(sink, -value)


@sink.command()
def show():
    """Show the volume"""
    import pulsectl
    with pulsectl.Pulse('volume-increaser') as pulse:
        for sink in config.sink:
            if sink.mute:
                print(f"{sink.description} muted")
            print(
                f"{sink.description}:\n\t{sink.volume.values[0]:.2f}, {sink.volume.values[1]:.2f}"
            )


@sink.command()
def toggle_mute():
    """Toggle mute"""
    import pulsectl
    with pulsectl.Pulse('volume-mute-toggler') as pulse:
        for sink in config.sink:
            pulse.mute(sink, not sink.mute)


@pulse.group()
@option("--source", type=SourceType(), multiple=True, help="The source to use")
def source(source):
    """Manipulate the sources"""
    import pulsectl
    with pulsectl.Pulse() as pulse:
        config.source = source or pulse.source_list()


@source.command()
def _set_as_default():
    """Set this source is the default one"""
    if len(config.source) > 1:
        raise click.UsageError("You must select at most one source")
    import pulsectl
    with pulsectl.Pulse() as pulse:
        pulse.default_set(config.source[0])


@source.command()
@argument("value", type=float, help="The value to use")
def __set(value):
    """Put the volume"""
    import pulsectl
    with pulsectl.Pulse() as pulse:
        for source in config.source:
            pulse.volume_set_all_chans(source, value)


@application.command()
def ipython():
    "Run ipython, to ease debugging"
    a = config.pulse.applications
    import IPython
    IPython.start_ipython(argv=[], user_ns=(globals() | locals()))
