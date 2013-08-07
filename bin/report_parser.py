#!/usr/bin/python

import sys
import datetime
import rfc822
import time

def get_datestr(l, hostname):
    split = l.split("host:")
    if len(split) > 1:
        datestr = split[0]
        rest=split[1].split(" ", 3)
        if len(rest) < 3:
            raise ValueError
        host = rest[0]
        pwd = rest[1].split(":")[1]
        try:
            hist = int(rest[2])
        except:
            hist = 0
        if (len(rest) == 4):
            cmd = rest[3]
        else:
            cmd = "n/a"
        return (datestr, host, pwd, hist, cmd)
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
    lines = [l for l in f.readlines()]
    summary = 0
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
        def get_day(date):
                return int(date.strftime("%d"))

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
            if first_time or last_week_print.strftime("%V") != start.strftime("%V"):
                print "sum: %0.2fh" % (summary/3600.)
                print
                print "WEEK %d" % week
                print "=================="
                last_week_print = start
                summary = 0

#            if int(last_start_date.strftime("%d")) != int(end.strftime("%d")):
            if first_time or get_day(last_day_print) != get_day(start):
                print start.strftime("== %a %b %d")
                first_time = False
                last_day_print = start

#            print "diff: %d %0.2fh" % (diff, )
            length = (time.mktime(end.timetuple()) -
                      time.mktime(start.timetuple()))
            if (length != 0):
                print " activity: %0.2fh %s -> %s (coming break %0.1fh)" % (
                    length/3600.,
                    last_start_date.strftime("%a %b %d %H:%M:%S"),
                    last_date.strftime("%a %b %d %H:%M:%S"),
                    break_time/3600.)
                summary += length

            last_start = epoc
            last_start_date = date
        last = epoc
        last_date = date

        if verbose:
            print datestr, host, pwd, cmd.strip()
    print "sum: %0.2fh" % (summary/3600.)

import argparse


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Process ps log.')
    parser.add_argument('-v', '--verbose', dest='verbose',
                action='store_true', help='Print all entries in pslog.')

    parser.add_argument("file", nargs="?", help="file containing pslogs")


    args = parser.parse_args()


    if args.file == None:
        f = sys.stdin
    else:
        f=file(args.file[0], "r")

    parse_report(f, args.verbose)
