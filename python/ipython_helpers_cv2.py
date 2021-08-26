#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from matplotlib import  pyplot as plt
import matplotlib as mpl
from matplotlib.widgets import Slider, Button, RadioButtons
import numpy as np
import cv2

from ipython_helpers_pylab2 import *

def mpl_cv2_image_mode_local():
    plt.tick_params(
        axis='both',
        which='both',
        bottom='off',
        top='off',
        left='off',
        right='off',
        labelbottom='off',
        labeltop='off',
        labelleft='off',
        labelright='off',
    )
    plt.grid()

def mpl_cv2_image_mode_global():
    mpl.rcParams["axes.grid"] = False
    mpl.rcParams["image.cmap"] = "gray"
    mpl_layout_bigger()

def imshow(image):
    if len(image.shape) == 2:
        return plt.imshow(image)
    else:
        plt.imshow(cv2.cvtColor(image, cv2.COLOR_BGR2RGB))

def points(rc_or_points):
    return np.concatenate(
        (
            rc_or_points[:, :, 1],
            rc_or_points[:, :, 0]
        ),
        axis=1).reshape(
            rc_or_points.shape
        )

def cvargwhere(mask):
    res = np.argwhere(mask)
    x, y = res.shape
    return points(res.reshape(x, 1, y))

def drawrectangle(i, points, *args, **kwargs):
    x, y, w, h = points
    cv2.rectangle(i, (x, y), (x + w, y + h), *args, **kwargs)

def drawhoughlinesp(img, lines):
    for x1,y1,x2,y2 in lines[0]:
        cv2.line(img,(x1,y1),(x2,y2), 255, 3)

def contourRectangle(img, *args, **kwargs):
    "returns (tl, tr, br, bl)"
    fast = cv2.FastFeatureDetector()
    kps = fast.detect(img)
    pts = map(lambda kp:kp.pt, kps)
    return np.array((
        max(pts, key=lambda pt: - pt[0] - pt[1]),
        max(pts, key=lambda pt: - pt[0] + pt[1]),
        max(pts, key=lambda pt: + pt[0] + pt[1]),
        max(pts, key=lambda pt: + pt[0] - pt[1]),
    ), *args, **kwargs)

# taken from
# http://www.pyimagesearch.com/2014/08/25/4-point-opencv-getperspective-transform-example/
def order_points(pts):
	# initialzie a list of coordinates that will be ordered
	# such that the first entry in the list is the top-left,
	# the second entry is the top-right, the third is the
	# bottom-right, and the fourth is the bottom-left
	rect = np.zeros((4, 2), dtype = "float32")

	# the top-left point will have the smallest sum, whereas
	# the bottom-right point will have the largest sum
	s = pts.sum(axis = 1)
	rect[0] = pts[np.argmin(s)]
	rect[2] = pts[np.argmax(s)]

	# now, compute the difference between the points, the
	# top-right point will have the smallest difference,
	# whereas the bottom-left will have the largest difference
	diff = np.diff(pts, axis = 1)
	rect[1] = pts[np.argmin(diff)]
	rect[3] = pts[np.argmax(diff)]

	# return the ordered coordinates
	return rect

# taken from
# http://www.pyimagesearch.com/2014/08/25/4-point-opencv-getperspective-transform-example/
def four_point_transform(image, pts):
	# obtain a consistent order of the points and unpack them
	# individually
	rect = order_points(pts)
	(tl, tr, br, bl) = rect

	# compute the width of the new image, which will be the
	# maximum distance between bottom-right and bottom-left
	# x-coordiates or the top-right and top-left x-coordinates
	widthA = np.sqrt(((br[0] - bl[0]) ** 2) + ((br[1] - bl[1]) ** 2))
	widthB = np.sqrt(((tr[0] - tl[0]) ** 2) + ((tr[1] - tl[1]) ** 2))
	maxWidth = max(int(widthA), int(widthB))

	# compute the height of the new image, which will be the
	# maximum distance between the top-right and bottom-right
	# y-coordinates or the top-left and bottom-left y-coordinates
	heightA = np.sqrt(((tr[0] - br[0]) ** 2) + ((tr[1] - br[1]) ** 2))
	heightB = np.sqrt(((tl[0] - bl[0]) ** 2) + ((tl[1] - bl[1]) ** 2))
	maxHeight = max(int(heightA), int(heightB))

	# now that we have the dimensions of the new image, construct
	# the set of destination points to obtain a "birds eye view",
	# (i.e. top-down view) of the image, again specifying points
	# in the top-left, top-right, bottom-right, and bottom-left
	# order
	dst = np.array([
		[0, 0],
		[maxWidth - 1, 0],
		[maxWidth - 1, maxHeight - 1],
		[0, maxHeight - 1]], dtype = "float32")

	# compute the perspective transform matrix and then apply it
	M = cv2.getPerspectiveTransform(rect, dst)
	warped = cv2.warpPerspective(image, M, (maxWidth, maxHeight))

	# return the warped image
	return warped

def downscale(img, maxwidth=512, maxheight=480):
    level = 0
    while img.shape[0] > maxwidth and img.shape[1] > maxheight:
        img = cv2.pyrDown(img)
        level += 1
    return img, level

def scanner(img):
    lowlevel, level = downscale(img)
    pts = contourRectangle(lowlevel)
    pts = pts * (2 ** level)
    return four_point_transform(
        img,
        pts,
    )
