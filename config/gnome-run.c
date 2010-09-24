/**********************************************************************
 * \file test.c
 *
 * \author Konubinix  (konubinix@gmail.com)
 * \date sam. 22:11:15 11/09/2010
 ***********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>

void die(const char *message)
{
	fputs(message, stderr);
	exit(1);
}

Atom get_atom(Display *display, const char *atom_name)
{
	Atom atom = XInternAtom(display, atom_name, False);
	if (atom == None)
		die("can't find an atom I need");
	return atom;
}

int main()
{
	Display *display;
	Atom gnome_panel_atom, run_atom;
	XClientMessageEvent event;

	display = XOpenDisplay(NULL);
	if (display == NULL)
		die("can't open display");

	gnome_panel_atom = get_atom(display, "_GNOME_PANEL_ACTION");
	run_atom = get_atom(display, "_GNOME_PANEL_ACTION_RUN_DIALOG");

	event.type = ClientMessage;
	event.window = DefaultRootWindow(display);
	event.message_type = gnome_panel_atom;
	event.format = 32;
	event.data.l[0] = run_atom;
	event.data.l[1] = (Time)0;

	XSendEvent(display, event.window, False, StructureNotifyMask,
	           (XEvent *)&event);

	XCloseDisplay(display);

	return 0;
}
