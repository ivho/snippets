#!/usr/bin/python

import sys
import datetime
import rfc822
import time
YEAR=2013

project_dirs = [("Totoro", ["/space/work/simics/totoro",
                            "/space/ivho/totoro"]),
                ("SVN", ["/space/work/simics/svn",
                         "/space/ivho/svn"])]
def pwd_to_proj(pwd):
    for (p, l) in project_dirs:
        for d in l:
            if pwd.startswith(d):
                return p
    return "other"

class Entry(object):
    def __init__(self, date, host, pwd, cmd):
        self.date = date
        self.host = host
        self.pwd = pwd
        self.cmd = cmd

    def __repr__(self):
        return "%s %s %s" % (self.date.__str__(), self.pwd, self.cmd)

class Activity():
    def __init__(self, start):
        self.start = start
        self.entries = []

    def set_end(self, end):
        self.end = end

    def add_entry_old(self, date, host, pwd, cmd):
        self.entries.append(Entry(date, host,pwd, cmd))

    def add_entry(self, entry):
        self.entries.append(entry)

    def length(self):
        return (time.mktime(self.end.timetuple()) -
                time.mktime(self.start.timetuple()))

    def __repr__(self):
        return "%0.2fh %s -> %s (%d entries)" % (
            self.length()/3600.,
            self.start.strftime("%H:%M"),
            self.end.strftime("%H:%M"),
            len(self.entries))

    def week(self):
        return int(self.start.strftime("%V"))

def parse_line(l):
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
        return Entry(date = parse_date(datestr),
                     host = host,
                     pwd = pwd,
                     cmd = cmd)
    return None

def parse_date(datestr):
    date = datetime.datetime.strptime(datestr, '%a %b %d %H:%M:%S %Z %Y ')
    return date

def all_entries(f):
    return [parse_line(l) for l in f.readlines()]

def get_epoch(date):
    return time.mktime(date.timetuple())

def get_project_activities(entries):
    proj = {}
    for prj in [prj for (prj, l) in project_dirs] + ["other"]:
        ee = [e for e in entries if pwd_to_proj(e.pwd) == prj]
        proj[prj] = get_activities(ee)
    return proj

BREAK_TIME=3600.0*2
def get_activities(entries):
    all_activities = []
    start = entries[0].date
    last = get_epoch(start)
    act = Activity(start)
    prev = start
    for entry in entries:
        act.add_entry(entry)
        break_time = get_epoch(entry.date) - get_epoch(prev)
        if break_time > BREAK_TIME:
            act.set_end(prev)
            all_activities.append(act)
            act = Activity(entry.date)
            start = entry.date
        prev = entry.date

    return all_activities

def parse_report(f, verbose):
    weeks=[]
    last = 0
    last_start = 0
    last_start_date = None
    ld = 0
    first_time = True
    lines = [l for l in f.readlines()]
    summary = 0
    entries = []
    all_activities = []
    act = None
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
        if act == None:
            act = Activity(date)

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
            act.set_end(end)
            all_activities.append(act)
            act = Activity(date)

#            if int(last_start_date.strftime("%V")) != int(end.strftime("%V")):
            if first_time or last_week_print.strftime("%V") != start.strftime("%V"):
#                print "sum: %0.2fh" % (summary/3600.)
#                print
#                print "WEEK %d" % week
#                print "=================="
                last_week_print = start
                summary = 0

#            if int(last_start_date.strftime("%d")) != int(end.strftime("%d")):
            if first_time or get_day(last_day_print) != get_day(start):
 #               print start.strftime("== %a %b %d")
                first_time = False
                last_day_print = start

#            print "diff: %d %0.2fh" % (diff, )
            length = (time.mktime(end.timetuple()) -
                      time.mktime(start.timetuple()))
            if (length != 0):
#                print " activity: %0.2fh %s -> %s (coming break %0.1fh)" % (
#                    length/3600.,
#                    last_start_date.strftime("%a %b %d %H:%M:%S"),
#                    last_date.strftime("%a %b %d %H:%M:%S"),
#                    break_time/3600.)
                summary += length

            last_start = epoc
            last_start_date = date
            entries = []
        act.add_entry(date, host, pwd, cmd)
        last = epoc
        last_date = date

        if verbose:
            print datestr, host, pwd, cmd.strip()
#   print "sum: %0.2fh" % (summary/3600.)
    return all_activities

def show_week(all_activities, week):
    for a in all_activities:
        if a.week() == week:
            print a, len(a.entries)
            for (date, host, pwd, cmd) in a.entries:
                print "  %s %s %s" % (date.strftime("%a %b %d %H:%M:%S"),
                                      host, pwd)
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

    all = parse_report(f, args.verbose)
    print "week 30"
    show_week(all, 30)
