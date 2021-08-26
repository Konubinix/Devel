#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sklearn as sk
import sklearn

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
    return sklearn.cross_validation.cross_val_score(
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

def sk_reshape_for_model(df, dim=1):
    if isinstance(df, pandas.Series):
        return numpy.array(df.reshape(len(df) / dim, dim))
    else:
        return numpy.array(df)

def sk_classification(clf, i, o, nx=500, ny=500, plot=True):
    xx, yy = numpy.meshgrid(
        numpy.linspace(i.min(), i.max(), nx),
        numpy.linspace(o.min(), o.max(), ny)
    )
    xxyy = c_[xx.ravel(), yy.ravel()]
    io = c_[i.ravel(), o.ravel()]
    clf.fit(io)
    z = clf.decision_function(xxyy).reshape(xx.shape)
    if plot:
        contour(xx, yy, z)
    return xx, yy, z
