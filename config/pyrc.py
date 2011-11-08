try:
    import readline
except ImportError:
    print "Module readline not available."
else:
    import rlcompleter
    class TabCompleter(rlcompleter.Completer):
        """Completer that supports indenting"""
        def complete(self, text, state):
            if not text:
                return ('    ', None)[state]
            else:
                return rlcompleter.Completer.complete(self, text, state)
    readline.set_completer(TabCompleter().complete)
    readline.parse_and_bind("tab: complete")
