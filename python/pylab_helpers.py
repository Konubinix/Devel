#!/usr/bin/env python
# -*- coding:utf-8 -*-

import itertools
import matplotlib
from matplotlib.pyplot import subplots
import numpy
import re
import pandas

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

def pd_lexicographic_lesser_than(vector1, vector2):
    if isinstance(vector1, pandas.core.frame.DataFrame):
        vector1 = vector1.iloc[:,0]
    if isinstance(vector2, pandas.core.frame.DataFrame):
        vector2 = vector2.iloc[:,0]
    difference = vector1 != vector2
    difference = difference[difference]
    if len(difference) == 0:
        # equal
        return 0
    first_different_index = difference.index[0]
    return 1 if vector1[first_different_index] < vector2[first_different_index] else -1

def pd_lexicographic_sort(df):
    # make a list of vectors out of df
    vector_list = [df[[c]] for c in df.columns]
    vector_list.sort(cmp=pd_lexicographic_lesser_than)
    return pandas.concat(vector_list, axis=1)

def asciify(k):
    return k.encode("ascii", errors="ignore").replace("?", "").replace(" ", "").replace("(", "").replace(")", "").replace("/", "")

def asciify_dataframe(dataframe):
    res = dataframe.applymap(
        lambda x: asciify(x)
        if isinstance(
                x,
                basestring)
        else x)
    res.columns = [asciify(c) for c in res.columns]
    return res

def pandas_put_into_hdf_tstable(dataframe, file_name, name="Values"):
    import tables
    import tstables
    f = tables.open_file(file_name, "a")
    dict_ = {"timestamp":tables.Int64Col(pos=0)}
    dict_.update({asciify(k): tables.Float64Col(pos=i+1) for i,k in enumerate(list(dataframe.columns))})
    MyClass = type("MyClass", (tables.IsDescription, ), dict_)
    ts = f.create_ts("/", asciify(name), MyClass)
    ts.append(asciify_dataframe(dataframe).astype("f8"))
    f.close()

def hdf_keys(hdf_file):
    import h5py
    file_ = h5py.File(hdf_file, mode="r")
    keys = file_.keys()
    file_.close()
    return keys

def pd_scale(input_dataframe, axis=0, with_mean=True, with_std=True):
    from sklearn.preprocessing import scale
    from pandas import DataFrame
    return DataFrame(scale(input_dataframe, axis=axis, with_mean=with_mean, with_std=with_std), index=input_dataframe.index, columns=input_dataframe.columns)

def pd_stripna(input_dataframe):
    return input_dataframe.dropna(axis=0, how="all").dropna(axis=1, how="all")

def mpl_get_color_cycle_from_color_map_name(number=10, name="coolwarm"):
    color = matplotlib.cm.get_cmap(name)(numpy.linspace(0.1,0.9,number)) # This returns RGBA; convert:
    return map(lambda rgb:'#%02x%02x%02x' % (rgb[0]*255,rgb[1]*255,rgb[2]*255),
               tuple(color[:,0:-1]))

def mpl_set_color_cycle_from_color_map_name(number=10, name="coolwarm"):
    """See mpl.cm and http://matplotlib.org/examples/color/colormaps_reference.html"""
    matplotlib.rcParams["axes.color_cycle"] = mpl_get_color_cycle_from_color_map_name(name=name, number=number)

mpl_set_color_cycle_from_color_map_name()

def mpl_show_colors_maps():
    """see http://wiki.scipy.org/Cookbook/Matplotlib/Show_colormaps"""
    a=outer(arange(0,1,0.01),ones(10))
    figure(figsize=(10,5))
    subplots_adjust(top=0.8,bottom=0.05,left=0.01,right=0.99)
    maps=[m for m in cm.datad if not m.endswith("_r")]
    maps.sort(key=lambda x:x.lower())
    l=len(maps)+1
    for i, m in enumerate(maps):
        subplot(1,l,i+1)
        axis("off")
        imshow(a,aspect='auto',cmap=get_cmap(m),origin="lower")
        title(m,rotation=90,fontsize=10)

def pd_display_size_no_limit():
    pandas.set_option('display.max_columns', None)
    pandas.set_option('display.max_rows', None)

def pd_display_size_reset():
    pandas.reset_option('display.max_columns')
    pandas.reset_option('display.max_rows')

def nb_set_fig_size():
    from IPython.html import widgets
    from IPython.display import display
    width, height = matplotlib.rcParams["figure.figsize"]
    width_slider = widgets.IntSlider(min=1, max=40, value=width)
    height_slider = widgets.IntSlider(min=1, max=40, value=height)
    def set_figsize(width, height):
        matplotlib.rcParams["figure.figsize"] = width, height
        matplotlib.pyplot.plot(numpy.sin(numpy.arange(100)))

    w = widgets.interactive(set_figsize,width=width_slider, height=height_slider)
    display(w)
