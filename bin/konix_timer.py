#!/usr/bin/python
# -*- coding: utf-8 -*-

import os
import gui
import thread
import time
import wx
import konix_notify
import functools
import rpyc
import logging
logging.basicConfig()
from rpyc.utils.server import ThreadedServer
from fysom import Fysom

a_lock = thread.allocate_lock()
time_widget = None
initial_time = 300
window = None
run = False
current_time = initial_time

EVT_CURRENT_TIME_ID = wx.NewId()

#################
## fsm helpers ##
#################
def events_leading_to(dst):
    return [event["name"] for event in fsm_events
            if event["dst"] == dst
        ]

def possible_events_leading_to(dst):
    return [name for name in events_leading_to(dst)
            if fsm.can(name)
        ]

########################
## Notification stuff ##
########################
def bell():
    konix_notify.main("ALARM !")
    try:
        import pygame
        pygame.init()
        pygame.mixer.music.load(
            os.path.join(
                os.environ["KONIX_DEVEL_DIR"],
                "data",
                "alarm.ogg"
            )
        )
        pygame.mixer.music.play()
    except:
        pass

#################
## State stuff ##
#################
fsm_onplaying_hooks = []
fsm_onpaused_hooks = []
fsm_onstopped_hooks = []

def fsm_onplaying(evt):
    for hook in fsm_onplaying_hooks:
        hook(evt)

def fsm_onpaused(evt):
    for hook in fsm_onpaused_hooks:
        hook(evt)

def fsm_onstopped(evt):
    for hook in fsm_onstopped_hooks:
        hook(evt)

fsm_events = [
    {'name': 'play', 'src': 'idle', 'dst': 'playing'},
    {'name': 'play', 'src': 'stopped', 'dst': 'playing'},
    {'name': 'pause', 'src': 'playing', 'dst': 'paused'},
    {'name': 'unpause', 'src': 'paused', 'dst': 'playing'},
    {'name': 'stop', 'src': 'playing', 'dst': 'stopped'},
    {'name': 'reset', 'src': 'paused', 'dst': 'stopped'},
]
fsm = Fysom({ 'initial': 'idle',
              'events': fsm_events,
              'callbacks': {
                 'onplaying': fsm_onplaying,
                 'onstopped': fsm_onstopped,
                 'onpaused': fsm_onpaused,
              }
          })

##,--------------
##| control stuff
##`--------------
control_post_hooks = []
def control_hook(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        res = func(*args, **kwargs)
        for post_hook in control_post_hooks:
            post_hook()
        return res
    return wrapper

@control_hook
def fsm_go_to(dst):
    event = possible_events_leading_to(dst)
    if event != []:
        return fsm.trigger(event[0])
    else:
        logging.error("Cannot go to %s" % dst)

def play():
    fsm_go_to("playing")

def pause():
    fsm_go_to("paused")

def stop():
    fsm_go_to("stopped")

###########################
## Time management stuff ##
###########################
timer_current_time_set_hooks = []
def set_current_time(current_time_):
    global current_time
    current_time = current_time_
    for hook in timer_current_time_set_hooks:
        hook(current_time)

# http://wiki.wxpython.org/LongRunningTasks
def timer():
    global time_widget, window, current_time
    while True:
        time.sleep(1)
        with a_lock:
            if run:
                current_time -= 1
                set_current_time(current_time)

def timer_onstopped_hook(evt):
    global current_time, run, initial_time
    with a_lock:
        run = False
        set_current_time(initial_time)

def timer_onplaying_hook(evt):
    global run, current_time, initial_time
    with a_lock:
        if run == True:
            set_current_time(initial_time)
        else:
            if evt.src != "paused":
                initial_time = current_time
        run = True

def timer_onpaused_hook(evt):
    global run
    with a_lock:
        run = False

fsm_onplaying_hooks.append(timer_onplaying_hook)
fsm_onpaused_hooks.append(timer_onpaused_hook)
fsm_onstopped_hooks.append(timer_onstopped_hook)

##########################
## Remote control stuff ##
##########################
class MyService(rpyc.Service):
    def exposed_play(self):
        play()
    def exposed_stop(self):
        stop()
    def exposed_pause(self):
        pause()
    def exposed_set_current_time(self, current_time):
        set_current_time(current_time)

def start_rpyc_server():
    server = ThreadedServer(MyService, port = 12345)
    server.start()

###############
## Gui stuff ##
###############
def gui_async_call(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        return wx.CallAfter(func, *args, **kwargs)
    return wrapper

def on_load(evt):
    print "initializing!"
    global time_widget, initial_time
    time_widget = evt.window['time']

def on_time_change(evt):
    global run, current_time
    current_time = int(
        eval(evt.window['time'].text) if evt.window['time'].text != "" else 0
    )
    if current_time <= 0 and run:
        run = False
        bell()

def on_time_keypress(evt):
    if evt.key == 13:
        play()

@gui_async_call
def update_graphics(evt=None):
    pause_button = gui.component.COMPONENTS["bgMin.pause"]
    stop_button = gui.component.COMPONENTS["bgMin.stop"]
    play_button = gui.component.COMPONENTS["bgMin.play"]
    state_label = gui.component.COMPONENTS["bgMin.state"]

    play_button.enabled = True if possible_events_leading_to("playing") else False
    stop_button.enabled = True if possible_events_leading_to("stopped") else False
    pause_button.enabled = True if possible_events_leading_to("paused") else False
    state_label.text = fsm.current

control_post_hooks.append(update_graphics)

def on_play_click(evt):
    play()

def on_pause_click(evt):
    pause()

def on_stop_click(evt):
    stop()

##,----------------------------------------
##| plug the gui control into the fsm stuff
##`----------------------------------------
@gui_async_call
def gui_set_current_time(current_time):
    global window
    with a_lock:
        window["time"].text = str(current_time)

timer_current_time_set_hooks.append(gui_set_current_time)

##########
## Main ##
##########
if __name__ == '__main__':
    thread.start_new_thread(start_rpyc_server, ())
    thread.start_new_thread(timer, ())
    window = gui.load()
    update_graphics()
    gui.main_loop()
