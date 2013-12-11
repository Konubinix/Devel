# make sure the tab command performs completion
import readline, rlcompleter
readline.parse_and_bind("tab: complete")

# handle history, taken from http://docs.python.org/2/tutorial/interactive.html
import atexit
import os

historyPath = os.path.expanduser("~/.pyhistory")
# read the previous history if possible
if os.path.exists(historyPath):
    readline.read_history_file(historyPath)

# save the history when the interpreter exits
def save_history(historyPath=historyPath):
    import readline
    readline.write_history_file(historyPath)
atexit.register(save_history)

del os, atexit, rlcompleter, readline, save_history, historyPath
