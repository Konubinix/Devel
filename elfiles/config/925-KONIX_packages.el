;;; 925-required-package.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sam

;; Author: sam <sam@konixwork>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; dist
(require 'savehist)
(require 'winner)
(require 'saveplace)

(setq-default
 straight-host-usernames
 '(
   (github    . "konubinix")
   (gitlab    . "konubinix")
   (bitbucket . "konubinix")
   )
 )
(setq-default straight-allow-recipe-inheritance t)
(setq-default use-package-always-defer t)
(setq-default use-package-always-ensure t) ;; only relevant when using package.el

(use-package auto-complete)
(use-package auto-scroll :straight (:fork t))
(use-package backup-dir)
(use-package counsel)
(use-package dap-mode)
(use-package dash)
(use-package dedicated)
(use-package default-text-scale)
(use-package delight)
(use-package dired-copy-paste :straight (:type git :host github :repo "jsilve24/dired-copy-paste"))
(use-package dired-quick-sort)
(use-package dockerfile-mode)
(use-package ement)
(use-package envrc)
(use-package earthfile-mode)
(use-package f)
(use-package feature-mode)
(use-package flycheck)
(use-package framemove)
(use-package git-timemachine :commands (git-timemachine))
(use-package golden-ratio)
(use-package go-mode)
(use-package hcl-mode :commands (hcl-mode))
(use-package highlight-parentheses)
(use-package highlight-symbol :commands (highlight-symbol))
(use-package image-roll :straight '(:type git :host github :repo "dalanicolai/image-roll.el"))
(use-package ini)
;; (use-package jl-encrypt)
(use-package js2-mode :commands (js2-mode))
(use-package keep-buffers)
(use-package kubel :commands (kubel))
(use-package kubernetes)
(use-package ledger-mode)
(use-package lsp-mode :commands (lsp))
(use-package lua-mode :commands (lua-mode))
(use-package mic-paren)
(use-package michelson :straight (:type git :host github :repo "MiloDavis/michelson-mode"))
(use-package miniedit :commands (miniedit))
(use-package multiple-cursors)
(use-package nix-mode :commands (nix-mode))
(use-package org :straight (:branch "release_9.5.5"))
(use-package ol-emacs-slack :straight (:type git :host github :repo "ag91/ol-emacs-slack"))
(use-package org-drill :bind-keymap  (("C-f D" . konix/org-drill-key-map)))
(use-package org-link-minor-mode :straight (:type git :host github :repo "seanohalpin/org-link-minor-mode"))
(use-package org-ql :commands (org-ql-search))
(use-package org-roam)
(use-package org-roam-bibtex)
(use-package org-search-goto)
(use-package org-transclusion)
(use-package origami :commands (origami-mode))
(use-package pdf-tools :straight (:repo "dalanicolai/pdf-tools" :branch "pdf-roll"))
(use-package plantuml-mode)
(use-package popwin)
(use-package prettier-js)
(use-package rust-mode)
(use-package slack :commands (slack-start) :straight (:fork t))
(use-package tempbuf)
(use-package terraform-mode)
(use-package tree-mode :commands (tree-mode))
(use-package typescript-mode)
(use-package uuidgen)
(use-package wgrep)
(use-package yaml-mode)
(use-package yasnippet)
(use-package zenburn-theme)
(use-package visible-mark :commands (visible-mark-mode))
(use-package undo-tree :commands (undo-tree-mode))
(use-package which-key)
(use-package hypothesis :straight (:type git :host github :repo "Konubinix/hypothesis"))

;; vendor
(require 'git-wip-mode nil t)
(require 'sticky-windows)
;; mine
(require 'KONIX_qutebrowser)

;; (use-package alert)
;; (use-package arduino-mode)
;; (use-package auto-complete)
;; (use-package backup-walker)
;; (use-package calfw)
;; (use-package calfw-gcal)
;; (use-package cask)
;; (use-package cheat-sh)
;; (use-package circe)
;; (use-package citeproc-org)
;; (use-package cmake-mode)
;; (use-package cquery)
;; (use-package csv-mode)
;; (use-package csv-mode)
;; (use-package dart-mode)
;; (use-package default-text-scale)
;; (use-package deft)
;; (use-package el-patch)
;; (use-package elf-mode)
;; (use-package elnode)
;; (use-package emacs-everywhere)
;; (use-package envrc)
;; (use-package git-wip-timemachine)
;; (use-package gited)
;; (use-package go-dlv)
;; (use-package graphviz-dot-mode)
;; (use-package groovy-mode)
;; (use-package highlight-parentheses)
;; (use-package hl-indent)
;; (use-package htmlize)
;; (use-package hydra)
;; (use-package hyperbole)
;; (use-package indium)
;; (use-package ivy)
;; (use-package ivy-bibtex)
;; (use-package ivy-xref)
;; (use-package jedi)
;; (use-package jedi-direx)
;; (use-package jupyter)
;; (use-package langtool)
;; (use-package lsp-java)
;; (use-package lsp-jedi)
;; (use-package lsp-mode)
;; (use-package markdown-mode)
;; (use-package maxframe)
;; (use-package mediawiki)
;; (use-package multiple-cursors)
;; (use-package notmuch)
;; (use-package ol-notmuch)
;; (use-package org)
;; (use-package org-contrib)
;; (use-package org-edna)
;; (use-package org-noter)
;; (use-package org-ql)
;; (use-package org-roam)
;; (use-package org-super-agenda)
;; (use-package orgalist)
;; (use-package orgalist)
;; (use-package ox-hugo)
;; (use-package phi-search)
;; (use-package phi-search-dired)
;; (use-package phi-search-mc)
;; (use-package poporg)
;; (use-package py-isort)
;; (use-package rainbow-mode)
;; (use-package realgud)
;; (use-package rg)
;; (use-package scad-mode)
;; (use-package scala-mode)
;; (use-package skewer-mode)
;; (use-package swiper)
;; (use-package tern)
;; (use-package vue-mode)
;; (use-package w3m)
;; (use-package wgrep)
;; (use-package which-key)
;; (use-package yapfify)


(provide '925-required-package)
;;; 925-required-package.el ends here
