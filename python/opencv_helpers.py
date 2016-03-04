#!/usr/bin/env python
# -*- coding:utf-8 -*-

from matplotlib import  pyplot as plt
import matplotlib as mpl
from pylab_helpers import *

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
