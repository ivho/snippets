#!/usr/bin/python

import sys
import datetime
import rfc822
import time

def get_datestr(l, hostname):
 #       print "<%s>" % l.strip()
        split = l.split("host:")
#        print "A:", len(split), split[0]
        if len(split) > 1:
            datestr = split[0]
 #           print "A:", split[1]
            rest=split[1].split(" ", 3)
#            print len(rest)
            if len(rest) != 4:
                raise ValueError
            host = rest[0]
            pwd = rest[1].split(":")[1]
            hist = int(rest[2])
            cmd = rest[3]
#            print "X:", rest[0], rest[1], rest[2]
#            print "Y:", rest[3]
            return (datestr, host, pwd, hist, cmd)
#        hostname = "nobu2"
        split = l.split(hostname)
#        print "B:", len(split), split[0]
#        print "B:", split[1]
        return None

def parse_date(datestr):
    date = datetime.datetime.strptime(datestr, '%a %b %d %H:%M:%S %Z %Y ')
    return date

def parse_report(f, verbose):
    weeks=[]
    last = 0
    last_start = 0
    last_start_date = None
    ld = 0
    first_time = True
    lines = [l for l in file(f).readlines()]
    for (i, l) in enumerate(lines):
        try:
            (datestr, host, pwd, hist, cmd) = get_datestr(l, "nobu2")
        except TypeError:
            continue
        try:
            date = parse_date(datestr)
        except ValueError:
            continue

        day=int(date.strftime("%d"))
        week=int(date.strftime("%V"))
        epoc = time.mktime(date.timetuple())
        break_time = epoc - last

        if last_start_date == None:
            last_start_date = date
            last_date = date
            ld = day
            last = epoc
            last_start = epoc

        if break_time > 3600*1 or i == len(lines)-1:
            # Found a break... print the last batch
            end = last_date
            start = last_start_date

#            if int(last_start_date.strftime("%V")) != int(end.strftime("%V")):
            if first_time or int(last_start_date.strftime("%V")) != week:
                print
                print "WEEK %d" % week
                print "=================="

#            if int(last_start_date.strftime("%d")) != int(end.strftime("%d")):
            if first_time or int(last_start_date.strftime("%d")) != day:
                print date.strftime("== %a %b %d")
		first_time = False

#            print "diff: %d %0.2fh" % (diff, )
            length = (time.mktime(end.timetuple()) -
                      time.mktime(start.timetuple()))
            if (length != 0):
                print " last: %0.2fh %s -> %s (prev break %0.1fh)" % (
                    length/3600.,
                    last_start_date.strftime("%a %b %d %H:%M:%S"),
                    last_date.strftime("%a %b %d %H:%M:%S"),
                    break_time/3600.)

            last_start = epoc
            last_start_date = date
        last = epoc
        last_date = date

#        if False:
        if verbose:
            print datestr, host, pwd

import argparse


if __name__ == "__main__":
	parser = argparse.ArgumentParser(description='Process ps log.')
	parser.add_argument('-v', '--verbose', dest='verbose',
			    action='store_true', help='Print all entries in pslog.')

	parser.add_argument("files", nargs="+", help="echo the string you use here")


	args = parser.parse_args()
	print args.verbose
	print args.files

	parse_report(args.files[0], args.verbose)
