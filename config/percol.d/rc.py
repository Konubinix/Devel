#!/usr/bin/env python3
# -*- coding:utf-8 -*-

percol.view.__class__.PROMPT = property(
    lambda self:
    r"<bold><blue>QUERY </blue>[a]:</bold> %q" if percol.model.finder.case_insensitive
    else r"<bold><green>QUERY </green>[A]:</bold> %q"
)

percol.view.CANDIDATES_LINE_BASIC = ("on_default", "default")
percol.view.CANDIDATES_LINE_SELECTED = ("underline", "on_magenta", "white")
percol.view.CANDIDATES_LINE_MARKED = ("bold", "on_cyan", "black")
percol.view.CANDIDATES_LINE_QUERY = ("yellow", "bold")
percol.view.MESSAGE_ERROR = ("on_red", "white")

# Emacs like
percol.import_keymap({
    "C-h" : lambda percol: percol.command.delete_backward_char(),
    "C-d" : lambda percol: percol.command.delete_forward_char(),
    "C-k" : lambda percol: percol.command.kill_end_of_line(),
    "C-y" : lambda percol: percol.command.yank(),
    "C-t" : lambda percol: percol.command.transpose_chars(),
    "C-a" : lambda percol: percol.command.beginning_of_line(),
    "C-e" : lambda percol: percol.command.end_of_line(),
    "C-b" : lambda percol: percol.command.backward_char(),
    "C-f" : lambda percol: percol.command.forward_char(),
    "M-f" : lambda percol: percol.command.forward_word(),
    "M-b" : lambda percol: percol.command.backward_word(),
    "M-d" : lambda percol: percol.command.delete_forward_word(),
    "M-h" : lambda percol: percol.command.delete_backward_word(),
    "C-n" : lambda percol: percol.command.select_next(),
    "C-p" : lambda percol: percol.command.select_previous(),
    "C-v" : lambda percol: percol.command.select_next_page(),
    "M-v" : lambda percol: percol.command.select_previous_page(),
    "M-<" : lambda percol: percol.command.select_top(),
    "M->" : lambda percol: percol.command.select_bottom(),
    "C-m" : lambda percol: percol.finish(),
    "C-j" : lambda percol: percol.finish(),
    "C-g" : lambda percol: percol.cancel(),
})
