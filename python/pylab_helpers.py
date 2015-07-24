#!/usr/bin/env python
# -*- coding:utf-8 -*-

import itertools
import matplotlib
from matplotlib.pyplot import subplots
from matplotlib import pyplot
import numpy
import re
import pandas
from mpldatacursor import datacursor

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

def mpl_wait_for_key_press(fig=None, key="enter", close=False):
    # I need to use an object. The pointer to the pause list will be captured by
    # closer in key_press_event. When the callback will change the pause
    # content, the outer function will see it. With the simple pause = True
    # code, the inner function would have changed a local copy of pause.
    if fig is None:
        fig = pyplot.gcf()
    pause = [True, ]
    def key_press_event(event):
        if event.key == key:
            pause[0] = False
    cid = fig.canvas.mpl_connect('key_press_event', key_press_event)
    while pause[0] and pyplot.fignum_exists(fig.number):
        fig.canvas.get_tk_widget().update()
    fig.canvas.mpl_disconnect(cid)
    if close and pyplot.fignum_exists(fig.number):
        pyplot.close(fig.number)

def mpl_show_colors_maps():
    """see http://wiki.scipy.org/Cookbook/Matplotlib/Show_colormaps"""
    a=numpy.outer(numpy.arange(0,1,0.01),numpy.ones(10))
    pyplot.figure(figsize=(10,5))
    pyplot.subplots_adjust(top=0.8,bottom=0.05,left=0.01,right=0.99)
    maps=[m for m in matplotlib.cm.datad if not m.endswith("_r")]
    maps.sort(key=lambda x:x.lower())
    l=len(maps)+1
    for i, m in enumerate(maps):
        pyplot.subplot(1,l,i+1)
        pyplot.axis("off")
        pyplot.imshow(a,aspect='auto',cmap=get_cmap(m),origin="lower")
        pyplot.title(m,rotation=90,fontsize=10)

def mpl_layout_bigger():
    matplotlib.rcParams["figure.subplot.left"] = 0
    matplotlib.rcParams["figure.subplot.right"] = 1
    matplotlib.rcParams["figure.subplot.bottom"] = 0
    matplotlib.rcParams["figure.subplot.top"] = 1
    matplotlib.rcParams["figure.subplot.wspace"] = 0
    matplotlib.rcParams["figure.subplot.hspace"] = 0

def mpl_layout_reset():
    matplotlib.rcParams["figure.subplot.left"] = matplotlib.rcParamsDefault["figure.subplot.left"]
    matplotlib.rcParams["figure.subplot.right"] = matplotlib.rcParamsDefault["figure.subplot.right"]
    matplotlib.rcParams["figure.subplot.bottom"] = matplotlib.rcParamsDefault["figure.subplot.bottom"]
    matplotlib.rcParams["figure.subplot.top"] = matplotlib.rcParamsDefault["figure.subplot.top"]
    matplotlib.rcParams["figure.subplot.wspace"] = matplotlib.rcParamsDefault["figure.subplot.wspace"]
    matplotlib.rcParams["figure.subplot.hspace"] = matplotlib.rcParamsDefault["figure.subplot.hspace"]

def mpl_click_coordinates_get(fig=None):
    fig = fig or pyplot.gcf()
    def onclick(event):
        print 'button=%d, x=%d, y=%d, xdata=%f, ydata=%f'%(
            event.button, event.x, event.y, event.xdata, event.ydata)
    global mpl_click_coordinates_get_cid
    mpl_click_coordinates_get_cid = fig.canvas.mpl_connect('button_press_event', onclick)
    return mpl_click_coordinates_get_cid

def mpl_click_coordinates_reset(cid=None, fig=None):
    fig = fig or pyplot.gcf()
    cid = cid or mpl_click_coordinates_get_cid
    fig.canvas.mpl_disconnect(cid)

def mpl_annotate_interactive(text, ax=None, arrowprops={"facecolor":'black', "shrink":0.05}):
    ax = ax or pyplot.gca()
    fig = ax.figure
    coordinates = [
        None, # xy
        None  # xytext
    ]
    pause = [True,]
    def get_xy(event):
        if coordinates[0] is None:
            coordinates[0] = (event.x, event.y)
            print("xy     : {}".format(coordinates[0]))
        else:
            coordinates[1] = (event.x, event.y)
            print("xytext : {}".format(coordinates[1]))
            pause[0] = False
    cid = fig.canvas.mpl_connect('button_press_event', get_xy)
    while pause[0] and pyplot.fignum_exists(fig.number):
        fig.canvas.get_tk_widget().update()
    fig.canvas.mpl_disconnect(cid)
    ax.annotate(
        text,
        xycoords='figure pixels',
        xy=coordinates[0],
        xytext=coordinates[1],
        arrowprops=arrowprops,
    )

def pd_display_size_no_limit():
    pandas.set_option('display.max_columns', None)
    pandas.set_option('display.max_rows', None)

def pd_display_size_set(max_columns=None, max_rows=None):
    if max_columns:
        pandas.set_option('display.max_columns', max_columns)
    else:
        pandas.reset_option('display.max_columns')
    if max_rows:
        pandas.set_option('display.max_rows', max_rows)
    else:
        pandas.reset_option('display.max_rows')

def pd_get_notnull_coordinates(dataframe_with_nan):
    """pd_get_coordinates(df[df >0])"""
    for column in dataframe_with_nan.columns:
        for row in dataframe_with_nan[column].dropna().index:
            yield (row, column)

def pd_heatmap(dataframe):
    ax = pyplot.imshow(dataframe.astype("f"), interpolation="none").get_axes()
    ax.set_xticklabels(dataframe.columns, rotation=90)
    ax.set_yticklabels(dataframe.index)
    ax.set_xticks(numpy.linspace(0, len(dataframe.columns) - 1, len(dataframe.columns)))
    ax.set_yticks(numpy.linspace(0, len(dataframe.index) - 1, len(dataframe.index)))

def pd_heatmap_correlation(dataframe):
    pd_heatmap(dataframe.corr())

def pd_like(dataframe_or_serie, values):
    if isinstance(dataframe_or_serie, pandas.Series):
        return pandas.Series(
            matplotlib.cbook.flatten(values),
            index=dataframe_or_serie.index
        )
    else:
        return pandas.DataFrame(
            values,
            index=dataframe.index,
            columns=dataframe.columns
        )

def pd_mpl_change():
    pandas.options.display.mpl_style = "default"

def pd_mpl_reset():
    pandas.options.display.mpl_style = None

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

def sk_nested_cross_val(model,
                        X,
                        y,
                        param_grid,
                        inner_cv=3,
                        outer_cv=10,
                        outer_n_jobs=-1,
                        verbose=0,
                        scoring="mean_squared_error"
):
    """
    model = RandomForestRegressor()
    sk_nested_cross_val(model, X, y, param_grid={"n_estimators":arange(10, 20)},)
    """
    clf = sklearn.grid_search.GridSearchCV(
        estimator=model,
        cv=inner_cv,
        verbose=verbose,
        param_grid=param_grid,
        scoring=scoring
    )

    # run the cross validation to find the score of the cross validated clf
    sklearn.cross_validation.cross_val_score(
        clf,
        X,
        y,
        cv=outer_cv,
        n_jobs=outer_n_jobs,
        verbose=verbose,
        scoring=scoring
    )

def sk_model_plot(i, o, model, model_name, save=True):
    model.fit(i, o)
    h = model.predict(i)
    e = h - o
    pyplot.clf()
    pyplot.subplot(211)
    pyplot.plot(i, o, ".", label="raw values")
    pyplot.plot(i, h, label=model_name)
    pyplot.legend()
    pyplot.gca().set_xlabel(i.columns[0])
    pyplot.gca().set_ylabel(o.columns[0])
    pyplot.draw()
    pyplot.subplot(212)
    e.plot(kind="hist", ax=pyplot.gca())
    pyplot.suptitle("{} vs {} (coef={}, intercept={})".format(i.columns[0], o.columns[0], model.coef_[0][0], model.intercept_[0]))
    if save:
        pyplot.savefig("{}_{}.png".format(i.columns[0], o.columns[0]))
