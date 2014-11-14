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
from rpyc.utils.server import ThreadedServer
from fysom import Fysom

a_lock = thread.allocate_lock()
time_widget = None
initial_time = None
window = None
run = False

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

###########################
## Time management stuff ##
###########################
class CurrentTime(wx.PyEvent):
    def __init__(self, current_time):
        wx.PyEvent.__init__(self)
        self.SetEventType(EVT_CURRENT_TIME_ID)
        self.current_time = current_time

def onCurrentTime(evt):
    global window
    with a_lock:
        window["time"].text = str(evt.current_time)

# http://wiki.wxpython.org/LongRunningTasks
def timer():
    global time_widget, window
    while True:
        time.sleep(1)
        with a_lock:
            if run:
                current_time = int(time_widget.text)
                current_time -= 1
                # indicate the current time
                wx.PostEvent(window.app, CurrentTime(current_time))

def onplaying(evt):
    global time_widget, run, initial_time
    with a_lock:
        if run == True:
            time_widget.text = str(initial_time)
        else:
            if evt.src != "paused":
                initial_time = int(time_widget.text)
        run = True

def onpaused(evt):
    global time_widget, run, initial_time
    with a_lock:
        run = False

def onstopped(evt):
    global time_widget, run
    with a_lock:
        run = False
        time_widget.text = str(initial_time)
        try:
            import pygame
            pygame.mixer.music.stop()
        except:
            pass

#################
## State stuff ##
#################
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
                 'onplaying': onplaying,
                 'onstopped': onstopped,
                 'onpaused': onpaused,
              }
          })

###################
## control stuff ##
###################
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
def play():
    event = possible_events_leading_to("playing")[0]
    fsm.trigger(event)

@control_hook
def pause():
    event = possible_events_leading_to("paused")[0]
    fsm.trigger(event)

@control_hook
def stop():
    event = possible_events_leading_to("stopped")[0]
    fsm.trigger(event)

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

def start_rpyc_server():
    server = ThreadedServer(MyService, port = 12345)
    server.start()

###############
## Gui stuff ##
###############
def on_load(evt):
    print "initializing!"
    global time_widget, initial_time, window
    time_widget = evt.window['time']
    # make sure the window will receive the current time event
    window = evt.window
    window.app.Connect(-1, -1, EVT_CURRENT_TIME_ID, onCurrentTime)
    initial_time = int(time_widget.text)
    thread.start_new_thread(timer, ())


def on_time_change(evt):
    global time_widget, run, initial_time
    current_time = int(evt.window['time'].text or 0)
    if current_time <= 0 and run:
        run = False
        bell()

def on_time_keypress(evt):
    if evt.key == 13:
        play()

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

##########
## Main ##
##########
if __name__ == '__main__':
    thread.start_new_thread(start_rpyc_server, ())
    window = gui.load()
    update_graphics()
    gui.main_loop()
