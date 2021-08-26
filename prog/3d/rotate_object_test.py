#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from Scientific.Geometry.Transformation import Rotation
from Scientific.Geometry.Quaternion import Quaternion
from Scientific.Geometry import Vector

from visual import *
import time

# display the axises
# x_axis = arrow(axis=vector(0,0,1))
# y_axis = arrow(axis=vector(0,1,0))
# z_axis = arrow(axis=vector(1,0,0))

# create the vector
# my_object = arrow(
#     axis=vector(3,3,3),
#     color=(0,1,1),
#     shaftwidth=0.01,
#     headwidth=0.05,
#     headlength=0.05,
# )
my_object = box(
    axis=vector(3,3,3),
    color=(0,1,1),
)
# my_object_label = label(
#     pos=my_object.pos,
#     text='The object'
# )

# my_new_object = arrow(
#     axis=vector(
#         0,
#         0,
#         0,
#     ),
#     color=(0,1,1),
#     shaftwidth=0.01,
#     headwidth=0.05,
#     headlength=0.05,
# )
my_new_object = box(
    axis=vector(
        0,
        0,
        0,
    ),
    color=(0,1,1),
    shaftwidth=0.01,
    headwidth=0.05,
    headlength=0.05,
)
my_new_object.visible = False

# test dragging axis
dragging_axis = arrow(
    axis=vector(1,0,1),
    color=(1,0,1),
    shaftwidth=0.01,
    headwidth=0.05,
    headlength=0.05,
)
dragging_axis_label = label(
    pos=dragging_axis.pos+dragging_axis.axis,
    text='The dragging axis',
)
dragging_axis.visible = False
dragging_axis_label.visible = False

# test rotation axis
rotation_axis = arrow(
    axis=vector(1,0,1),
    color=(1,1,0),
    shaftwidth=0.01,
    headwidth=0.05,
    headlength=0.05,
)
rotation_axis_label = label(
    pos=rotation_axis.pos,
    text='The rotation axis',
)
rotation_axis.visible = False
rotation_axis_label.visible = False

# rotate
# r1 = Rotation(
#     Vector(
#         rotation_axis.axis.x,
#         rotation_axis.axis.y,
#         rotation_axis.axis.z,
#     ),
#     3.14/3 # 60°
# )
# q1 = r1.asQuaternion()
# v1 = Vector(
#     my_object.axis.x,
#     my_object.axis.y,
#     my_object.axis.z,
# )
# new_v1 = r1(v1)

# scene
scene.range = 5 # fixed size, no autoscaling
mouse_pos = None
dragging = None
while True:
    if scene.mouse.events:
        m = scene.mouse.getevent()
        if m.press:
            mouse_pos = scene.mouse.project(normal=(0,0,1))
        if m.drag and mouse_pos != None:
            dragging = True
            #dragging_axis.visible = True
            #rotation_axis.visible = True
            dragging_axis.pos.x = mouse_pos.x
            dragging_axis.pos.y = mouse_pos.y
            dragging_axis.pos.z = mouse_pos.z
            rotation_axis.pos.x = mouse_pos.x
            rotation_axis.pos.y = mouse_pos.y
            rotation_axis.pos.z = mouse_pos.z
            dragging_axis.axis.x = 0
            dragging_axis.axis.y = 0
            dragging_axis.axis.z = 0
            rotation_axis.axis.x = 0
            rotation_axis.axis.y = 0
            rotation_axis.axis.z = 0
            my_new_object.axis.x = my_object.axis.x
            my_new_object.axis.y = my_object.axis.y
            my_new_object.axis.z = my_object.axis.z
            my_new_object.visible = True
            my_object.visible = False

        elif m.drop:
            mouse_pos = None
            dragging = None
            dragging_axis.visible = False
            dragging_axis_label.visible = False
            rotation_axis.visible = False
            rotation_axis_label.visible = False
            my_new_object.visible = False
            my_object.visible = True
            my_object.axis.x = my_new_object.axis.x
            my_object.axis.y = my_new_object.axis.y
            my_object.axis.z = my_new_object.axis.z
            my_object.up.x = my_new_object.up.x
            my_object.up.y = my_new_object.up.y
            my_object.up.z = my_new_object.up.z
    elif dragging:
        new_mouse_pos = scene.mouse.project(normal=(0,0,1))
        diff = new_mouse_pos - mouse_pos
        dragging_axis.axis.x = diff.x
        dragging_axis.axis.y = diff.y
        dragging_axis.axis.z = 0
        rotation_axis.axis.x = -diff.y
        rotation_axis.axis.y = diff.x
        rotation_axis.axis.z = 0
        # rotate the vector around the rotation axis of
        # length(rotation_axis)/360 * 10 degrees
        length = rotation_axis.axis.mag * (3.14 * 2.0) / 15.0
        #print "Rotation of angle:", length
        r1 = Rotation(
            Vector(
                rotation_axis.axis.x,
                rotation_axis.axis.y,
                rotation_axis.axis.z,
            ),
            length
        )
        v1 = Vector(
            my_object.axis.x,
            my_object.axis.y,
            my_object.axis.z,
        )
        new_v1 = r1(v1)
        # rotate the up also
        up = Vector(
            my_object.up.x,
            my_object.up.y,
            my_object.up.z,
        )
        new_up = r1(up)
        my_new_object.up.x = new_up.x()
        my_new_object.up.y = new_up.y()
        my_new_object.up.z = new_up.z()

        my_new_object.axis.x = new_v1.x()
        my_new_object.axis.y = new_v1.y()
        my_new_object.axis.z = new_v1.z()

    time.sleep(0.08)
