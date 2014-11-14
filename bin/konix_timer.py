#!/usr/bin/python
# -*- coding: utf-8 -*-

import os
import gui
import thread
import time
import wx
import konix_notify
import rpyc
from rpyc.utils.server import ThreadedServer

a_lock = thread.allocate_lock()
time_widget = None
initial_time = None
window = None
run = False

EVT_CURRENT_TIME_ID = wx.NewId()

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

def start():
    global time_widget, run, initial_time
    with a_lock:
        if run == True:
            time_widget.text = str(initial_time)
        else:
            initial_time = int(time_widget.text)
        run = True

def stop():
    global time_widget, run
    with a_lock:
        run = False
        time_widget.text = str(initial_time)
        try:
            import pygame
            pygame.mixer.music.stop()
        except:
            pass

##########################
## Remote control stuff ##
##########################
class MyService(rpyc.Service):
    def exposed_start(self):
        start()
    def exposed_stop(self):
        stop()

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
        start()

def on_start_click(evt):
    start()

def on_stop_click(evt):
    stop()

##########
## Main ##
##########
if __name__ == '__main__':
    thread.start_new_thread(start_rpyc_server, ())
    window = gui.load()
    gui.main_loop()
