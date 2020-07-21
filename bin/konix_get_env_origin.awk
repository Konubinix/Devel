#!/usr/bin/env konix_awk.sh

BEGIN {
	key = ARGV[1]
	delete ARGV[1] # or awk will try to interpret it as a file
	value = ARGV[2]
	delete ARGV[2] # or awk will try to interpret it as a file
}
{
	if($0 ~ "^DEBUG:root:## Parsing.+") {
		confline = $0
		conffile = gensub(/DEBUG:root:## Parsing (.+)/, "\033[35m\\1\033[0m", "g", confline)
	}
	if($0 ~ "^DEBUG:root:## Attempting to include env from.+") {
		confline = $0
		conffile = gensub(/DEBUG:root:## Attempting to include env from (.+) (\(.+\))/, "\033[35m\\1\033[0m", "g", confline)
	}
	if($0 ~ "DEBUG:root:analysing \\[" key "\\] =.+" value) {
		variable = gensub(/DEBUG:root:analysing [[](.+)[]].+/, "\\1", "g", $0)
		print confline
		print "In " conffile
		line = $0
		print "\033[36m" key "\033[0m" " = " "\033[34m" value "\033[0m"
		# gsub(key, "\033[36m" key "\033[0m", line)
		# gsub(value, "\033[34m" value "\033[0m", line)
		print line
	}
}
