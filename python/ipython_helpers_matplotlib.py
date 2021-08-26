#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import matplotlib
import numpy
import matplotlib.pyplot as plt
from matplotlib.widgets import Slider, Button
from functools import partial
from collections import namedtuple
Interval = namedtuple("Interval", ("label", "valmin", "valmax"))

# def explort_imshow(f, sliders_properties):


# taken from https://zulko.wordpress.com/2012/08/18/a-gui-for-the-exploration-of-functions-with-python-matplotlib/
def explort(f, sliders_properties):
    nVars = len(sliders_properties)
    slider_width = 1.0/nVars
    print(slider_width)
    # CREATE THE CANVAS
    global oldax
    oldax = plt.gca()
    figure = plt.figure("sliders")
    # choose an appropriate height
    width,height = figure.get_size_inches()
    height = min(0.5*nVars,8)
    figure.set_size_inches(width,height,forward = True)
    # CREATE THE SLIDERS
    sliders = []
    for i, properties in enumerate(sliders_properties):
        ax = plt.axes([0.1 , 0.95-0.9*(i+1)*slider_width,
                       0.8 , 0.8* slider_width])
        sliders.append(Slider(ax=ax, **properties.__dict__))
    # CREATE THE CALLBACK FUNCTIONS
    plt.sca(oldax)
    def on_changed(event) :
        plt.sca(oldax)
        res = f(*(s.val for s in sliders))
        plt.draw_if_interactive()
    def on_key_press(event):
        if event.key is 'enter':
            on_changed(event)
    figure.canvas.mpl_connect('key_press_event', on_key_press)
    for s in sliders:
        s.on_changed(on_changed)

def mpl_get_color_cycle_from_color_map_name(number=10, name="cubehelix"):
    color = matplotlib.cm.get_cmap(name)(numpy.linspace(0.1,0.9,number)) # This returns RGBA; convert:
    return list(map(lambda rgb:'#{:02x}{:02x}{:02x}'.format(
        int(rgb[0]*255),
        int(rgb[1]*255),
        int(rgb[2]*255)
    ),
               tuple(color[:,0:-1])))

def mpl_set_color_cycle_from_color_map_name(number=10, name="cubehelix"):
    """See mpl.cm and http://matplotlib.org/examples/color/colormaps_reference.html"""
    matplotlib.rcParams["axes.color_cycle"] = mpl_get_color_cycle_from_color_map_name(name=name, number=number)

mpl_set_color_cycle_from_color_map_name()
