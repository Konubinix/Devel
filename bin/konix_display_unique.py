#!/usr/bin/python3
import sys
import konix_notify

if __name__ == "__main__":
	if len(sys.argv) <= 1:
		exit(1)
	message = ' '.join(sys.argv[1:])
	konix_notify.main(message,True)
