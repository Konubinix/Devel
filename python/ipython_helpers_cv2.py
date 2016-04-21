#!/usr/bin/env python
# -*- coding:utf-8 -*-

from matplotlib import  pyplot as plt
import matplotlib as mpl
from matplotlib.widgets import Slider, Button, RadioButtons
import numpy as np
import cv2

from ipython_helpers_pylab2 import *

def mpl_cv2_image_mode_local():
    plt.tick_params(
        axis='both',
        which='both',
        bottom='off',
        top='off',
        left='off',
        right='off',
        labelbottom='off',
        labeltop='off',
        labelleft='off',
        labelright='off',
    )
    plt.grid()

def mpl_cv2_image_mode_global():
    mpl.rcParams["axes.grid"] = False
    mpl.rcParams["image.cmap"] = "gray"
    mpl_layout_bigger()

def imshow(image):
    if len(image.shape) == 2:
        return plt.imshow(image)
    else:
        plt.imshow(cv2.cvtColor(image, cv2.COLOR_BGR2RGB))

def points(rc_or_points):
    return np.concatenate(
        (
            rc_or_points[:, :, 1],
            rc_or_points[:, :, 0]
        ),
        axis=1).reshape(
            rc_or_points.shape
        )

def cvargwhere(mask):
    res = np.argwhere(mask)
    x, y = res.shape
    return points(res.reshape(x, 1, y))

def drawrectangle(i, points, *args, **kwargs):
    x, y, w, h = points
    cv2.rectangle(i, (x, y), (x + w, y + h), *args, **kwargs)

def drawhoughlinesp(img, lines):
    for x1,y1,x2,y2 in lines[0]:
        cv2.line(img,(x1,y1),(x2,y2), 255, 3)
