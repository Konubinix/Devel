#!/usr/bin/env python
# -*- coding:utf-8 -*-

import GPy
from sklearn.base import BaseEstimator
from abc import ABCMeta, abstractmethod

import six

class GP(six.with_metaclass(ABCMeta, BaseEstimator)):
    def fit(self, X, y, n_jobs=1):
        ker = GPy.kern.Matern52(X.shape[1],ARD=True) + GPy.kern.White(X.shape[1])
        self.gp = GPy.models.GPRegression(X, y, ker)
        return self

    def predict(self, X):
        return self.gp.predict(X)[0]
