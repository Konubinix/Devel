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

(require 'savehist)
(require 'etags)
(require 'winner)
(require 'saveplace)
(require 'paren)

(use-package region-bindings-mode :ensure t :defer t)
(use-package popwin :ensure t :defer t)
(use-package mic-paren :ensure t :defer t)
(use-package uuidgen :ensure t :defer t)
(use-package golden-ratio :ensure t)
(use-package counsel :ensure t :defer t)
(use-package miniedit :ensure t :defer t :commands (miniedit))
(use-package yasnippet :ensure t :defer t)
(use-package mic-paren :ensure t :defer t)
(use-package zenburn-theme :ensure t :defer t)
(use-package slack :ensure t :commands (slack-start))

(require 'framemove)
(require 'qutebrowser)
(require 'backup-dir)
(require 'sticky-windows)
(require 'tempbuf)
(require 'keep-buffers)
(require 'git-wip-mode nil t)

(load-theme 'zenburn)
(use-package highlight-parentheses :ensure t :defer t)


(use-package org-drill
  :ensure nil
  :bind (
         ("C-f D d" . org-drill)
         ("C-f D r" . org-drill-resume)
         ("C-f D h" . konix/org-drill-heading)
         )
  )

;; (use-package alert :ensure nil :defer t)
;; (use-package arduino-mode :ensure nil :defer t)
;; (use-package auto-complete :ensure nil :defer t)
;; (use-package backup-walker :ensure nil :defer t)
;; (use-package calfw :ensure nil :defer t)
;; (use-package calfw-gcal :ensure nil :defer t)
;; (use-package cask :ensure nil :defer t)
;; (use-package cheat-sh :ensure nil :defer t)
;; (use-package circe :ensure nil :defer t)
;; (use-package citeproc-org :ensure nil :defer t)
;; (use-package cmake-mode :ensure nil :defer t)
;; (use-package cquery :ensure nil :defer t)
;; (use-package csv-mode :ensure nil :defer t)
;; (use-package csv-mode :ensure nil :defer t)
;; (use-package dart-mode :ensure nil :defer t)
;; (use-package default-text-scale :ensure nil :defer t)
;; (use-package deft :ensure nil :defer t)
;; (use-package dockerfile-mode :ensure nil :defer t)
;; (use-package el-patch :ensure nil :defer t)
;; (use-package elf-mode :ensure nil :defer t)
;; (use-package elnode :ensure nil :defer t)
;; (use-package emacs-everywhere :ensure nil :defer t)
;; (use-package envrc :ensure nil :defer t)
;; (use-package earthfile-mode :ensure nil :defer t)
;; (use-package feature-mode :ensure nil :defer t)
;; (use-package flycheck :ensure nil :defer t)
;; (use-package git-timemachine :ensure nil :defer t)
;; (use-package git-wip-timemachine :ensure nil :defer t)
;; (use-package gited :ensure nil :defer t)
;; (use-package gnus-alias :ensure nil :defer t)
;; (use-package go-dlv :ensure nil :defer t)
;; (use-package go-mode :ensure nil :defer t)
;; (use-package graphviz-dot-mode :ensure nil :defer t)
;; (use-package groovy-mode :ensure nil :defer t)
;; (use-package hcl-mode :ensure nil :defer t)
;; (use-package highlight-parentheses :ensure nil :defer t)
;; (use-package highlight-symbol :ensure nil :defer t)
;; (use-package hl-indent :ensure nil :defer t)
;; (use-package htmlize :ensure nil :defer t)
;; (use-package hydra :ensure nil :defer t)
;; (use-package hyperbole :ensure nil :defer t)
;; (use-package indium :ensure nil :defer t)
;; (use-package ivy :ensure nil :defer t)
;; (use-package ivy-bibtex :ensure nil :defer t)
;; (use-package ivy-xref :ensure nil :defer t)
;; (use-package jedi :ensure nil :defer t)
;; (use-package jedi-direx :ensure nil :defer t)
;; (use-package js2-mode :ensure nil :defer t)
;; (use-package jupyter :ensure nil :defer t)
;; (use-package kubel :ensure nil :defer t)
;; (use-package langtool :ensure nil :defer t)
;; (use-package ledger-mode :ensure nil :defer t)
;; (use-package lsp-java :ensure nil :defer t)
;; (use-package lsp-jedi :ensure nil :defer t)
;; (use-package lsp-mode :ensure nil :defer t)
;; (use-package lua-mode :ensure nil :defer t)
;; (use-package markdown-mode :ensure nil :defer t)
;; (use-package maxframe :ensure nil :defer t)
;; (use-package mediawiki :ensure nil :defer t)
;; (use-package multiple-cursors :ensure nil :defer t)
;; (use-package nix-mode :ensure nil :defer t)
;; (use-package notmuch :ensure nil :defer t)
;; (use-package ol-notmuch :ensure nil :defer t)
;; (use-package org :ensure nil :defer t)
;; (use-package org-contrib :ensure nil :defer t)
;; (use-package org-edna :ensure nil :defer t)
;; (use-package org-noter :ensure nil :defer t)
;; (use-package org-ql :ensure nil :defer t)
;; (use-package org-roam :ensure nil :defer t)
;; (use-package org-super-agenda :ensure nil :defer t)
;; (use-package orgalist :ensure nil :defer t)
;; (use-package orgalist :ensure nil :defer t)
;; (use-package ox-hugo :ensure nil :defer t)
;; (use-package pdf-tools :ensure nil :defer t)
;; (use-package phi-search :ensure nil :defer t)
;; (use-package phi-search-dired :ensure nil :defer t)
;; (use-package phi-search-mc :ensure nil :defer t)
;; (use-package plantuml-mode :ensure nil :defer t)
;; (use-package poporg :ensure nil :defer t)
;; (use-package prettier-js :ensure nil :defer t)
;; (use-package py-isort :ensure nil :defer t)
;; (use-package rainbow-mode :ensure nil :defer t)
;; (use-package realgud :ensure nil :defer t)
;; (use-package rg :ensure nil :defer t)
;; (use-package rust-mode :ensure nil :defer t)
;; (use-package scad-mode :ensure nil :defer t)
;; (use-package scala-mode :ensure nil :defer t)
;; (use-package skewer-mode :ensure nil :defer t)
;; (use-package swiper :ensure nil :defer t)
;; (use-package tern :ensure nil :defer t)
;; (use-package terraform-mode :ensure nil :defer t)
;; (use-package tree-mode :ensure nil :defer t)
;; (use-package typescript-mode :ensure nil :defer t)
;; (use-package undo-tree :ensure nil :defer t)
;; (use-package vue-mode :ensure nil :defer t)
;; (use-package w3m :ensure nil :defer t)
;; (use-package wgrep :ensure nil :defer t)
;; (use-package which-key :ensure nil :defer t)
;; (use-package yaml-mode :ensure nil :defer t)
;; (use-package yapfify :ensure nil :defer t)

(use-package quelpa-use-package
  :ensure t
  )

(use-package ement
  :quelpa (ement :fetcher github :repo "alphapapa/ement.el")
  :ensure t
  )


(provide '925-required-package)
;;; 925-required-package.el ends here
