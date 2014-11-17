#!/usr/bin/env python
# -*- coding:utf-8 -*-

import cmd
import rpyc

class KonixTimerController(cmd.Cmd, object):
    def __init__(self):
        cmd.Cmd.__init__(self)
        self.do_connect(None)

    def do_connect(self, line):
        self.conn = rpyc.connect("localhost", 12345)
        print "Connected to the timer"

    def do_play(self, line):
        self.conn.root.play()

    def do_pause(self, line):
        self.conn.root.pause()

    def do_stop(self, line):
        self.conn.root.stop()

    def do_EOF(self, line):
        return True

if __name__ == "__main__":
    program = KonixTimerController()
    program.cmdloop()
