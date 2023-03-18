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

(setq-default straight-base-dir (expand-file-name "elfiles/deps/straight" perso-dir))
(defvar bootstrap-version)
(let ((bootstrap-file
          (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
         (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
                "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq-default straight-use-package-by-default t)
(setq-default straight-fix-flycheck )

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
;; straight strongly suggests to set it to nil
(setq-default use-package-always-ensure nil)
(setq-default use-package-verbose t)

(setq-default straight-recipe-overrides nil)
;; (straight-override-recipe '(org :branch "release_9.5.5"))

;; best setting according to the author
(setq-default straight-check-for-modifications '(watch-files find-when-checking))

(let (time_before_load time_after_load diff_time diff_abs_time)
    (setq time_before_load (current-time))

    (use-package arduino-mode)
    (use-package argdown :straight (:type built-in) :mode ("\\.argdown\\'" . argdown-mode))
    (use-package authinfo :straight (:type built-in) :mode ("netrc.gpg" . authinfo-mode))
    (use-package auto-complete)
    (use-package auto-scroll :commands (auto-scroll-mode))
    (use-package backup-dir)
    (use-package backup-walker)
    (use-package bbdb)
    (use-package citeproc)
    (use-package copilot :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el")))
    (use-package counsel)
    ;; lsp needs to be setup before dap
    (use-package lsp-mode :commands (lsp))
    (use-package dap-mode)
    (use-package dash)
    (use-package dedicated)
    (use-package default-text-scale)
    (use-package delight)
    (use-package diff :straight (:type built-in) :mode ("COMMIT_EDITMSG". diff-mode))
    (use-package dired-copy-paste :straight (:type git :host github :repo "jsilve24/dired-copy-paste"))
    (use-package dired-filetype-face)
    (use-package dired-quick-sort)
    (use-package dockerfile-mode)
    (use-package earthfile-mode :straight (:fork t))
    (use-package editorconfig)
    (use-package edit-indirect)
    (use-package ement :straight (:repo "alphapapa/ement.el"))
    (use-package envrc)
    (use-package f)
    (use-package feature-mode)
    (use-package flycheck)
    (use-package framemove)
    (use-package git-timemachine :commands (git-timemachine))
    (use-package git-wip-timemachine)
    (use-package gnus-alias)
    (use-package gitri :straight (:type built-in) :mode ("git-rebase-todo" . gitri-mode))
    (use-package go-dlv)
    (use-package go-mode :mode "_helpers.tpl")
    (use-package golden-ratio)
    (use-package hcl-mode :commands (hcl-mode))
    (use-package highlight-parentheses)
    (use-package highlight-symbol :commands (highlight-symbol))
    (use-package konix/org-gtd-context-edit-mode :straight (:type built-in) :mode "gtd_contexts/")
    (use-package hydra)
    (use-package hypothesis :commands (konix/hypothesis-archive) :straight (:type git :host github :repo "Konubinix/hypothesis"))
    (use-package image-roll :straight '(:type git :host github :repo "dalanicolai/image-roll.el"))
    (use-package ini)
    (use-package js2-mode :commands (js2-mode))
    (use-package keep-buffers)
    (use-package key-chord)
    (use-package lisp :straight (:type built-in) :mode ("emacs$" . list-mode))
    (use-package kubel :commands (kubel))
    (use-package kubernetes)
    (use-package ledger-mode :commands (konix/ledger-run konix/ledger-report))
    (use-package lsp-jedi)
    (use-package lua-mode :commands (lua-mode) :mode "\\.lua$")
    (use-package mic-paren)
    (use-package michelson :straight (:type git :host github :repo "MiloDavis/michelson-mode"))
    (use-package miniedit :commands (miniedit))
    (use-package multiple-cursors)
    (use-package nix-mode :commands (nix-mode))
    (use-package ol-emacs-slack :straight (:type git :host github :repo "ag91/ol-emacs-slack"))
    (use-package ol-notmuch)
    (use-package org :mode ("\\.org_archive" . org-mode))
    (use-package org-checklist :straight org-contrib)
    (use-package org-edna)
    (use-package org-drill :bind-keymap  ("C-f D" . konix/org-drill-key-map))
    (use-package org-link-minor-mode :straight (:type git :host github :repo "seanohalpin/org-link-minor-mode"))
    (use-package org-mime :commands (konix/org-mime-htmlize-current))
    (use-package org-ql :commands (org-ql-search))
    (use-package org-roam :commands (konix/org-roam-note))
    (use-package org-roam-bibtex)
    (use-package org-search-goto)
    (use-package org-super-agenda)
    (use-package org-transclusion)
    (use-package origami :commands (origami-mode))
    (use-package ox-hugo)
    (use-package pdf-tools :straight (:repo "dalanicolai/pdf-tools" :branch "pdf-roll") :magic ("%PDF" . pdf-view-mode))
    (use-package phi-search-dired)
    (use-package plantuml-mode)
    (use-package popwin)
    (use-package python :straight (:type built-in) :mode (("\\.py\\'" . python-mode) ("Tiltfile" . python-mode)))
    (use-package prettier-js)
    (use-package py-isort)
    (use-package region-bindings-mode :commands (region-bindings-mode-enable))
    (use-package rust-mode)
    (use-package scad-mode)
    (use-package slack :commands (slack-start) :straight (:fork t) :bind-keymap ("<f2> l" . konix/slack-global-map))
    (use-package tempbuf)
    (use-package terraform-mode)
    (use-package tracking)
    (use-package tree-mode :commands (tree-mode))
    (use-package typescript-mode)
    (use-package undo-tree :commands (undo-tree-mode))
    (use-package uuidgen)
    (use-package visible-mark :commands (visible-mark-mode))
    (use-package wgrep)
    (use-package ws-butler)
    (use-package which-key)
    (use-package yaml-mode)
    (use-package yapfify)
    (use-package yasnippet)
    (use-package zenburn-theme)

    (setq time_after_load (current-time))
    (setq diff_time (time-subtract time_after_load time_before_load))
    (setq diff_abs_time (time-subtract time_after_load *emacs-load-start*))
    (message "%ss, %sms, %sµs: Package loaded in %ss, %sms and %sµs"
        (second diff_abs_time)
        (/ (third diff_abs_time) 1000)
        (mod (third diff_abs_time) 1000)
        (second diff_time)
        (/ (third diff_time) 1000)
        (mod (third diff_time) 1000)
        )
    )


(provide '925-required-package)
;;; 925-required-package.el ends here
