#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import gptools
from sklearn.base import BaseEstimator
from abc import ABCMeta, abstractmethod

import six

class GP(six.with_metaclass(ABCMeta, BaseEstimator)):
    def fit(self, X, y, n_jobs=1):
        self.gp = gptools.gaussian_process.GaussianProcess(
            gptools.Matern52Kernel(X.shape[1]),
            X=X,
            y=y)
        return self

    def predict(self, X):
        return self.gp.predict(X)[0]
