#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import itertools
import matplotlib
from matplotlib.pyplot import subplots
from matplotlib import pyplot
import numpy
import re
import pandas
import traceback
import sys
try:
    from sklearn_heplers import *
except:
    print("Could not import sklearn_helpers")
try:
    from mpldatacursor import datacursor
except:
    print("Could not import mpldatacursor")
try:
    from odo import odo
except:
    print("Could not import odo")

idx = pandas.IndexSlice

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
    >>> ndtype = numpy.dtype([('a', '<i4'), ('b', [('ba', '<f8'), ('bb', '<i4')])])
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

def _pd_upsample_month_to_days_generator(df):
    for row in [pandas.DataFrame(df.loc[r] / float(r.day)) for r in df.index]:
        yield pandas.DataFrame(
            row.iloc[:, 0].to_dict(),
            index=pandas.date_range(
                row.columns[0].strftime("%Y-%m-01"),
                row.columns[0].strftime("%Y-%m-%d")
            ),
            columns=row.index)

def pd_upsample_month_to_days(df):
    return pandas.concat(_pd_upsample_month_to_days_generator(df))

def pd_prettytable(df):
    from prettytable import PrettyTable
    table = PrettyTable([''] + list(df.columns))
    for row in df.itertuples():
        table.add_row(row)
    return str(table)

def pd_prettytable_no_index(df):
    from prettytable import PrettyTable
    table = PrettyTable(list(df.columns))
    for row in df.itertuples():
        table.add_row(row[1:])
    return str(table)

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

def pd_put_into_hdf_tstable(dataframe, file_name, name="Values"):
    import tables
    import tstables
    f = tables.open_file(file_name, "a")
    dict_ = {"timestamp":tables.Int64Col(pos=0)}
    dict_.update({asciify(k): tables.Float64Col(pos=i+1) for i,k in enumerate(list(dataframe.columns))})
    MyClass = type("MyClass", (tables.IsDescription, ), dict_)
    ts = f.create_ts("/", asciify(name), MyClass)
    ts.append(asciify_dataframe(dataframe).astype("f8"))
    f.close()

def pd_scale(input_dataframe, axis=0, with_mean=True, with_std=True):
    from sklearn.preprocessing import scale
    from pandas import DataFrame
    return DataFrame(scale(input_dataframe, axis=axis, with_mean=with_mean, with_std=with_std), index=input_dataframe.index, columns=input_dataframe.columns)

def pd_stripna(input_dataframe):
    return input_dataframe.dropna(axis=0, how="all").dropna(axis=1, how="all")

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
        print('button=%d, x=%d, y=%d, xdata=%f, ydata=%f'%(
            event.button, event.x, event.y, event.xdata, event.ydata))
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
            index=dataframe_or_serie.index,
            columns=dataframe_or_serie.columns
        )

def pd_mpl_change():
    pandas.options.display.mpl_style = "default"
pd_mpl_change()

def pd_fill_between(dataframe, *args, **kwargs):
    assert dataframe.shape[1] == 2, "Bad value for the shape"
    pyplot.fill_between(
        dataframe.index,
        dataframe.ix[:, 0],
        dataframe.ix[:, 1],
        *args,
        **kwargs
    )

pandas.DataFrame.fill_between = pd_fill_between

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

def pd_index_intersections(i, o):
    ind = i.index.intersection(o.index)
    i = i.reindex(ind)
    o = o.reindex(ind)
    return i, o

def pd_movingaverage(df, window_size=None):
    result = df.copy()
    window_size = window_size if window_size else result.shape[0] / 10.
    window = numpy.ones(int(window_size))/float(window_size)
    for c in result.columns:
        result[c] = numpy.convolve(result[c], window, 'same')
    return result

def close_all():
    pyplot.close("all")

def pd_rolling_mean(df, window_size=None):
    window_size = window_size if window_size else df.shape[0] / 10.
    return df.rolling.mean(window_size)

pandas.DataFrame.rolm=pd_rolling_mean
pandas.Series.rolm=pd_rolling_mean

def pd_plotm(arg, window_size=None, *args, **kwargs):
    return arg.rolm(window_size).plot(*args, **kwargs)

pandas.DataFrame.plotm = pd_plotm
pandas.Series.plotm = pd_plotm

mpl_toggle_visibility_numbers = None
def mpl_toggle_visibility(ax=None):
    """
http://matplotlib.org/examples/event_handling/legend_picking.html
"""
    ax = ax if ax else pyplot.gca()
    fig = ax.get_figure()
    global mpl_toggle_visibility_numbers
    if mpl_toggle_visibility_numbers:
        for number in mpl_toggle_visibility_numbers:
            fig.canvas.mpl_disconnect(number)
        mpl_toggle_visibility_numbers = None
        print("Deactivated")
        return
    lines = ax.get_lines()
    lined = dict()
    lined2 = dict()
    leg = ax.get_legend()
    for legline, origline in zip(leg.get_lines(), lines):
        legline.set_picker(5)  # 5 pts tolerance
        origline.set_picker(5)
        lined[legline] = origline
        lined2[origline] = legline

    def toggle_visibility(origline, legline, vis=None):
        vis = vis if vis is not None else not origline.get_visible()
        origline.set_visible(vis)
        # Change the alpha on the line in the legend so we can see what lines
        # have been toggled
        if vis:
            legline.set_alpha(1.0)
        else:
            legline.set_alpha(0.2)

    def onscroll(event):
        if event.button == "up":
            for origline, legline in lined2.items():
                toggle_visibility(origline, legline, True)
        elif event.button == "down":
            for origline, legline in lined2.items():
                toggle_visibility(origline, legline, False)
        pyplot.draw_if_interactive()

    def onclick(event):
        if event.button == 3: # right click
            mpl_zoom_on_visible_lines()

    def onpick(event):
        # on the pick event, find the orig line corresponding to the
        # legend proxy line, and toggle the visibility
        if not event.mouseevent.button in [1, 2]: # right click
            return

        legline = event.artist
        if not legline in lined:
            # not a legline, thus a normal line
            origline = legline
            #epick it only if visible (making a invisible line appear is
            #strange)
            if not origline.get_visible():
                return
            legline = lined2[origline]
        else:
            origline = lined[legline]

        if event.mouseevent.button == 2: # middle click
            to_toggle = [(origline2, legline2)
             for origline2, legline2 in lined2.items()
             if origline2 != origline
             and origline2.get_visible()
            ]
            if to_toggle:
                [toggle_visibility(origline2, legline2)
                 for origline2, legline2 in to_toggle
                ]
            else:
                [toggle_visibility(origline2, legline2)
                 for origline2, legline2 in lined2.items()
                 if origline2 != origline
                ]
        else:
            toggle_visibility(origline, legline)

        fig.canvas.draw()

    mpl_toggle_visibility_numbers = (
        fig.canvas.mpl_connect('pick_event', onpick),
        fig.canvas.mpl_connect('button_press_event', onclick),
        fig.canvas.mpl_connect('scroll_event', onscroll),
    )
    print("Activated")
    return mpl_toggle_visibility_numbers

def mpl_zoom_on_line(line):
    ax = line.get_axes()
    xmax, ymax=line.get_xydata().max(axis=0)
    xmin, ymin=line.get_xydata().min(axis=0)
    pyplot.axis([xmin, xmax, ymin, ymax])

def mpl_zoom_on_visible_lines(ax=None):
    ax = ax if ax else pyplot.gca()
    xmax, ymax = numpy.array([
        line.get_xydata().max(axis=0)
        for line in ax.get_lines()
        if line.get_visible()]).max(axis=0)
    xmin, ymin = numpy.array([
        line.get_xydata().min(axis=0)
        for line in ax.get_lines()
        if line.get_visible()]).min(axis=0)
    pyplot.axis([xmin, xmax, ymin, ymax])

mpl_toggle_zoom_on_line_numbers = None
def mpl_toggle_zoom_on_line(ax=None):
    ax = ax if ax else pyplot.gca()
    fig = ax.get_figure()
    global mpl_toggle_zoom_on_line_numbers
    if mpl_toggle_zoom_on_line_numbers:
        for number in mpl_toggle_zoom_on_line_numbers:
            fig.canvas.mpl_disconnect(mpl_toggle_zoom_on_line_number)
        mpl_toggle_zoom_on_line_numbers = None
        print("Deactivated")
        return

    for line in ax.get_lines():
        line.set_picker(5)

    def onpick(event):
        if event.mouseevent.button == 1:
            line = event.artist
            mpl_zoom_on_line(line)

    def onclick(event):
        if event.button == 3: # right click
            pyplot.axis("auto")

    mpl_toggle_zoom_on_line_numbers = (
        fig.canvas.mpl_connect('pick_event', onpick),
        fig.canvas.mpl_connect('button_press_event', onclick),
    )
    print("Activated")
    return mpl_toggle_zoom_on_line_numbers

mpl_rotation_X_axes = None
def mpl_rotation_X(wait_for_key="enter"):
    global mpl_rotation_X_axes
    if mpl_rotation_X_axes:
        pyplot.gcf().delaxes(mpl_rotation_X_axes)
        pyplot.draw()
        mpl_rotation_X_axes = None
        return
    from matplotlib.widgets import Slider
    ax = pyplot.gca()
    mpl_rotation_X_axes = pyplot.axes([0.25, 0.1, 0.65, 0.03],)
    rotation_slider = Slider(mpl_rotation_X_axes, 'Rotation', -90.0, 90.0, valinit=5)
    def set_rotation(rotation):
        for label in ax.get_xticklabels():
            label.set_rotation(rotation)
    rotation_slider.on_changed(set_rotation)
    # must be returned : cf https://github.com/matplotlib/matplotlib/issues/3105/
    if wait_for_key:
        mpl_wait_for_key_press(key=wait_for_key)
        # to deactivate it
        mpl_rotation_X()
    return mpl_rotation_X_axes, rotation_slider

def pd_fill_between(df1, df2, columns=None):
    columns = columns or df1.columns
    for column in columns:
        pyplot.fill_between(
            df1.index,
            df1[column],
            df2[column]
        )

def odo_sql_circular_foreign_keys_workaround():
    import sqlalchemy as sa
    from datashape import discover
    @discover.register(sa.ForeignKey, sa.sql.FromClause)
    def do_not_discover_foreign_key_relationship(fk, parent, parent_measure=None):
        return {}

    import odo.backends.sql
    odo.backends.sql.discover_foreign_key_relationship = do_not_discover_foreign_key_relationship
    print("You can now load databases with circular foreign keys")

def nb_setup_plotly_cufflinks():
    from plotly.offline import download_plotlyjs, init_notebook_mode
    import cufflinks
    download_plotlyjs("https://cdn.plot.ly/plotly-latest.min.js")
    init_notebook_mode()
    cufflinks.go_offline()
