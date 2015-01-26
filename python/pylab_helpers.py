#!/usr/bin/env python
# -*- coding:utf-8 -*-

import itertools
import matplotlib
from matplotlib.pyplot import subplots
import numpy

def timeseries_scatter_plots(inputs, names=None, title=None, nrows=4):
  """inputs: matrix of measures
  each line is a measure. len(inputs) is the number of measures. len(inputs[0])
  is the number of dimensions of the measure.

This function plots the scatter plot comparing the distribution of point for each
dimension compared with other dimensions
  """
  inputs = numpy.array(inputs)
  len_of_input = len(inputs[0])
  names = names or inputs.dtype.names or [str(i) for i in xrange(len_of_input)]
  assert names is None or len_of_input == len(names), (
    "The names and the inputs must have the same length"
  )

  ij = [
    (i, j)
    for i, j in itertools.product(
        range(len_of_input),
        range(len_of_input))
    if i < j]

  ncols = len(ij) / nrows + 1
  f, sp = subplots(nrows, ncols)
  if title:
    f.suptitle(title)
  for a, (i,j) in zip(sp.flat, ij):
    a.scatter(inputs[:, i], inputs[:, j])
    a.set_title("{} vs {}".format(names and names[i] or i,
                                  names and names[j] or j))

def timeseries_plot_all(inputs, names=None, title=None, nrows=4):
  """inputs: matrix of measures
  each line is a measure. len(inputs) is the number of measures. len(inputs[0])
  is the number of dimensions of the measure.

This function plots all measured series according to each dimension. X values
are the occurrence, Y values are the measured values. Each plot represents a
dimension.
  """
  len_of_input = len(inputs[0])
  names = names or inputs.dtype.names or [str(i) for i in xrange(len_of_input)]
  assert len_of_input == len(names), (
      "The names and the inputs must have the same length"
  )
  nrows = min(nrows, len_of_input)
  ncols = ( (len_of_input - 1) / nrows ) + 1
  f, sp = subplots(nrows, ncols)
  if isinstance(sp, matplotlib.axes.Axes):
      sp = numpy.array([sp,])
  if title:
      f.suptitle(title)
  for figure, input, name in zip(sp.flat, inputs.T, names):
      figure.plot(input)
      figure.set_title(name)
