#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from decimal import Decimal as D


def balance(amounts):
    _amounts = {
        key: D(str(value)) for key, value in amounts.items()
    }
    res = []
    while _amounts:
        m, M = 0, 0
        mkey = Mkey = None
        for key, value in _amounts.items():
            if value > M:
                M = value
                Mkey = key
            if -value > m:
                m = -value
                mkey = key
        if M > m:
            transaction = (Mkey, mkey, m)
            _amounts.pop(mkey)
            _amounts[Mkey] -= m
        elif M < m:
            transaction = (Mkey, mkey, M)
            _amounts.pop(Mkey)
            _amounts[mkey] += M
        else:
            transaction = (Mkey, mkey, M)
            _amounts.pop(Mkey)
            _amounts.pop(mkey)
        res.append(transaction)
    return res


def format(transactions):
    res = ""
    for transaction in transactions:
        res += f"""{transaction[0]} gives {transaction[2]} to {transaction[1]}
"""
    return res
