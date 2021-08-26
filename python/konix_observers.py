#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from collections import defaultdict

class Observer(object):
    def __init__(self):
        self.id = None
        self.hooks = []

observers = defaultdict(Observer)

if __name__ == "__main__":
    def hook_on_i():
        from matplotlib import pyplot as plt
        plt.imshow(i)
        plt.draw_if_interactive()
    observers["i"].hooks.append(hook_on_i)
