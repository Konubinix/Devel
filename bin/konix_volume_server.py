#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from SimpleXMLRPCServer import SimpleXMLRPCServer
from SimpleXMLRPCServer import SimpleXMLRPCRequestHandler
import os
import subprocess

class RequestHandler(SimpleXMLRPCRequestHandler):
    rpc_paths = ('/RPC2',)

# Create server
server = SimpleXMLRPCServer(("0.0.0.0", 9633),
                            requestHandler=RequestHandler)
server.register_introspection_functions()

def get_volume():
    """Get the volume"""
    p=subprocess.Popen(
        ["konix_get_volume.sh"],
        stdout=subprocess.PIPE)
    return int(p.stdout.read())
server.register_function(get_volume)

def set_volume(volume):
    """Get the volume"""
    os.system("konix_set_volume.sh {}".format(volume))
    return True
server.register_function(set_volume)

server.serve_forever()
