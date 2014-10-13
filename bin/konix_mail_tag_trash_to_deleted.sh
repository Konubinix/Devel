#!/bin/bash

notmuch tag +deleted -- not tag:deleted and \( $(konix_notmuch_trash_folders_query.py) \)
