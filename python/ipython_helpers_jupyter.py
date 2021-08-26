#!/usr/bin/env python3
# -*- coding:utf-8 -*-


def jupyter_edit(content, cell_type="code"):
    """
Use with a custom.js code that looks like

var save_to_edit = function (env) {
	var cell = IPython.notebook.get_selected_cell();
	callbacks = {
		iopub : {
			output : function() {
				cell.set_text(arguments[0].content.text);
			}
		}
	};
	env.notebook.kernel.execute("jupyter_edit('''"+ cell.get_text() +"''')", callbacks);
}

require(["base/js/events", "base/js/utils"], function (events, utils) {
    events.on('notebook_loaded.Notebook', function() {
		var add_command_shortcuts = {
			'alt-s' : {
				help    : 'Save the current cell to be edited',
				help_index : 'zz',
				handler : function (env) {
					save_to_edit(env)
					return false;
				}
			}
		}
		IPython.keyboard_manager.command_shortcuts.add_shortcuts(add_command_shortcuts);
	})
});
- cell_type: "code", "raw" and "markdown"
"""
    import os
    import subprocess
    import sys
    import tempfile
    type_to_extension = {
        "code": "py",
        "raw": "txt",
        "markdown": "md",
    }
    suffix = ".{}".format(type_to_extension[cell_type])
    tf = tempfile.NamedTemporaryFile(suffix=suffix, delete=False)
    tf.write(content.encode("utf-8"))
    tf.close()
    subprocess.call([os.environ.get("EDITOR", "editor"), tf.name])
    content = open(tf.name, "r").read().decode("utf-8")
    sys.stdout.write(content)
    os.remove(tf.name)


def start_kernel_and_console():
    import os

    import zmq

    pid = os.fork()
    if pid == 0:
        # child
        from IPython import embed_kernel

        dict_ = globals()
        import inspect
        frame = inspect.currentframe()
        local_ns = frame.f_back.f_locals
        dict_.update(local_ns)
        embed_kernel(local_ns=dict_)
    else:
        import time
        time.sleep(2)
        from jupyter_console.app import main
        main(argv=["--existing"])
