#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from scipy.interpolate import Rbf

from sklearn.base import BaseEstimator
from abc import ABCMeta, abstractmethod

import six

class RBF(six.with_metaclass(ABCMeta, BaseEstimator)):
    def fit(self, X, y, n_jobs=1):
        values = [a for a in X.T] + [y.ravel(),]
        self.rbf = Rbf(*values, function="gaussian")
        return self

    def predict(self, X):
        values = [a for a in X.T]
        return self.rbf(*values)
