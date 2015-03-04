#!/usr/bin/env python
# -*- coding:utf-8 -*-

import itertools
import matplotlib
from matplotlib.pyplot import subplots
import numpy
import re

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

def viewasfloatarray(array):
    return array.view(dtype="f").reshape(len(array), len(array[0]))

def array_center(array):
    """Return a new array, whose mean is 0 on the axis=0."""
    return array - array.mean(axis=0)

def array_reduce(array):
    """Return a new array, whose values are in the interval [0-1]. The maximum
    and minimum values taken into account are along the axis=0."""
    M = array.max(axis=0)
    m = array.min(axis=0)
    return (array - m) / (M - m)

def recfunctions_flatten_descr(ndtype, joinchar=None, __parents=None):
    """
    Flatten a structured data-type description.

    Examples
    --------
    >>> from numpy.lib import recfunctions as rfn
    >>> ndtype = np.dtype([('a', '<i4'), ('b', [('ba', '<f8'), ('bb', '<i4')])])
    >>> rfn.flatten_descr(ndtype)
    (('a', dtype('int32')), ('ba', dtype('float64')), ('bb', dtype('int32')))
    >>> rfn.flatten_descr(ndtype, joinchar="_")
    (('a', dtype('int32')), ('b_ba', dtype('float64')), ('b_bb', dtype('int32')))

    """
    names = ndtype.names
    if not __parents:
        __parents = []
    if joinchar and __parents:
        prefix = joinchar.join(__parents) + joinchar
    else:
        prefix = ""
    if ndtype.subdtype:
        typ, (number, ) = ndtype.subdtype
        descr = []
        for index in range(1, number + 1):
            descr.append(
                ("{}{}".format(prefix, index), typ)
            )
        return tuple(descr)
    if names is None:
        return ndtype.descr
    else:
        descr = []
        for field in names:
            (typ, _) = ndtype.fields[field]
            if typ.names or typ.subdtype:
                descr.extend(recfunctions_flatten_descr(
                    typ,
                    joinchar,
                    __parents + [field ,]))
            else:
                descr.append((prefix + field, typ))
        return tuple(descr)

def recfunctions_view_flatten_array(array, joinchar=None):
    return array.view(
        numpy.dtype(list(recfunctions_flatten_descr(array.dtype, joinchar)))
    )

# def recfunctions_filter_descr(ndtype, nameregexp):
#     if isinstance(nameregexp, basestring):
#         nameregexp = re.compile(nameregexp)
#     if not ndtype.names:
#         return ndtype
#     names = ndtype.names
#     if names is None:
#         return ndtype, True
#     else:
#         descr = []
#         for field in names:
#             (typ, _) = ndtype.fields[field]
#             res, keepit = recfunctions_filter_descr(typ, nameregexp)
#             if res is not None and keepit:
#                 descr.append((field, res))
#         if descr:
#             return dtype(descr)
#         else:
#             return None
