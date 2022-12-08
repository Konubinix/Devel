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

(setq-default use-package-always-ensure t)

;; elpa/melpa/etc
(use-package auto-complete :defer t)
(use-package counsel :defer t)
(use-package dap-mode :defer t)
(use-package dash :defer t)
(use-package default-text-scale :defer t)
(use-package delight)
(use-package dockerfile-mode :defer t)
(straight-use-package '(ement :type git :host github :repo "alphapapa/ement.el"))
(use-package envrc :defer t)
(use-package earthfile-mode :defer t)
(use-package f :defer t)
(use-package flycheck :defer t)
(use-package gherkin-mode :defer t)
(use-package git-timemachine :commands (git-timemachine))
(use-package golden-ratio)
(use-package go-mode :defer t)
(use-package hcl-mode :commands (hcl-mode))
(use-package highlight-parentheses :defer t)
(use-package highlight-symbol :commands (highlight-symbol))
(straight-use-package '(image-roll :type git :host github :repo "dalanicolai/image-roll.el"))
(straight-use-package '(ini :type git :host github :repo "daniel-ness/ini.el"))
(use-package js2-mode :commands (js2-mode))
(use-package kubel :commands (kubel))
;; (straight-use-package '(kubel :type git :host github :repo "abrochard/kubel" :fork t))
(use-package kubernetes :defer t)
(use-package ledger-mode :defer t)
(use-package lsp-mode :commands (lsp))
(use-package lua-mode :commands (lua-mode))
(use-package mic-paren :defer t)
(use-package mic-paren :defer t)
(straight-use-package '(michelson :type git :host github :repo "MiloDavis/michelson-mode"))
(use-package miniedit :defer t :commands (miniedit))
(use-package multiple-cursors :defer t)
(use-package nix-mode :commands (nix-mode))
(straight-use-package '(ol-emacs-slack :type git :host github :repo "ag91/ol-emacs-slack"))
;; explicitly use org in a stable version because otherwise it would be
;; automatically pulled by hypothesis on master
(straight-use-package '(org :type git :url "https://git.savannah.gnu.org/git/emacs/org-mode.git" :branch "release_9.5.5"))
;; (use-package org :defer t)
(use-package org-drill
  :bind-keymap  (
                 ("C-f D" . konix/org-drill-key-map)
                 )
  )
(straight-use-package '(org-link-minor-mode :type git :host github :repo "seanohalpin/org-link-minor-mode"))
(use-package org-ql :commands (org-ql-search))
(use-package org-roam :defer t)
(use-package org-roam-bibtex :defer t)
(use-package org-transclusion :defer t)
(use-package origami :commands (origami-mode))
;; version of pdf-tools that has continuous scrolling
(straight-use-package '(pdf-tools :type git :host github :repo "dalanicolai/pdf-tools" :branch "pdf-roll"))
(use-package popwin :defer t)
(use-package prettier-js :defer t)
(use-package rust-mode :defer t)
;; (use-package slack :commands (slack-start))
(straight-use-package '(slack :type git :host github :repo "yuya373/emacs-slack"
                              :fork t))
(use-package terraform-mode :defer t)
(use-package tree-mode :commands (tree-mode))
(use-package typescript-mode :defer t)
(use-package uuidgen :defer t)
(use-package wgrep :defer t)
(use-package yaml-mode :defer t)
(use-package yasnippet :defer t)
(use-package zenburn-theme :defer t)
(use-package visible-mark :commands (visible-mark-mode))
(use-package undo-tree :commands (undo-tree-mode))
(use-package which-key :defer t)
(use-package windata :defer t) ;; needed by imenu-tree
;; need to be used after using org
(straight-use-package '(hypothesis :type git :host github :repo "Konubinix/hypothesis"))

;; vendor
(require 'framemove)
(require 'qutebrowser)
(require 'backup-dir)
(require 'sticky-windows)
(require 'tempbuf)
(require 'keep-buffers)
(require 'git-wip-mode nil t)


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
;; (use-package el-patch :ensure nil :defer t)
;; (use-package elf-mode :ensure nil :defer t)
;; (use-package elnode :ensure nil :defer t)
;; (use-package emacs-everywhere :ensure nil :defer t)
;; (use-package envrc :ensure nil :defer t)
;; (use-package feature-mode :ensure nil :defer t)
;; (use-package git-wip-timemachine :ensure nil :defer t)
;; (use-package gited :ensure nil :defer t)
;; (use-package go-dlv :ensure nil :defer t)
;; (use-package graphviz-dot-mode :ensure nil :defer t)
;; (use-package groovy-mode :ensure nil :defer t)
;; (use-package highlight-parentheses :ensure nil :defer t)
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
;; (use-package jupyter :ensure nil :defer t)
;; (use-package langtool :ensure nil :defer t)
;; (use-package lsp-java :ensure nil :defer t)
;; (use-package lsp-jedi :ensure nil :defer t)
;; (use-package lsp-mode :ensure nil :defer t)
;; (use-package markdown-mode :ensure nil :defer t)
;; (use-package maxframe :ensure nil :defer t)
;; (use-package mediawiki :ensure nil :defer t)
;; (use-package multiple-cursors :ensure nil :defer t)
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
;; (use-package phi-search :ensure nil :defer t)
;; (use-package phi-search-dired :ensure nil :defer t)
;; (use-package phi-search-mc :ensure nil :defer t)
;; (use-package plantuml-mode :ensure nil :defer t)
;; (use-package poporg :ensure nil :defer t)
;; (use-package py-isort :ensure nil :defer t)
;; (use-package rainbow-mode :ensure nil :defer t)
;; (use-package realgud :ensure nil :defer t)
;; (use-package rg :ensure nil :defer t)
;; (use-package scad-mode :ensure nil :defer t)
;; (use-package scala-mode :ensure nil :defer t)
;; (use-package skewer-mode :ensure nil :defer t)
;; (use-package swiper :ensure nil :defer t)
;; (use-package tern :ensure nil :defer t)
;; (use-package vue-mode :ensure nil :defer t)
;; (use-package w3m :ensure nil :defer t)
;; (use-package wgrep :ensure nil :defer t)
;; (use-package which-key :ensure nil :defer t)
;; (use-package yapfify :ensure nil :defer t)


(provide '925-required-package)
;;; 925-required-package.el ends here
