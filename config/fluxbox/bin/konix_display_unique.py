#!/usr/bin/python
import sys
import display

if __name__ == "__main__":
	if len(sys.argv) <= 1:
		exit(1)
	message = ' '.join(sys.argv[1:])
	display.main(message,True)
