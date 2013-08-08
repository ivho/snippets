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

def get_epoch(date):
    return time.mktime(date.timetuple())

class ShellActivityParser(object):
    def __init__(self, filename, break_time):
        f = file(filename, "r")
        self.entries = []
        for l in f.readlines():
            e = self.parse_line(l)
            if e != None:
                self.entries.append(e)
        print "LEN", len(self.entries)
        print self.entries[0]
        self.break_time = break_time

    def parse_line(self, l):
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
            return Entry(date = self.parse_date(datestr),
                         host = host,
                         pwd = pwd,
                         cmd = cmd)
        return None

    def parse_date(self, datestr):
        date = datetime.datetime.strptime(datestr, '%a %b %d %H:%M:%S %Z %Y ')
        return date


    def get_project_activities(self, entries):
        proj = {}
        for prj in [prj for (prj, l) in project_dirs] + ["other"]:
            ee = [e for e in entries if pwd_to_proj(e.pwd) == prj]
            proj[prj] = get_activities(ee)
        return proj

    def get_activities(self):
        all_activities = []
        start = self.entries[0].date
        last = get_epoch(start)
        act = Activity(start)
        prev = start
        for entry in self.entries:
            act.add_entry(entry)
            break_time = get_epoch(entry.date) - get_epoch(prev)
            if break_time > self.break_time:
                act.set_end(prev)
                all_activities.append(act)
                act = Activity(entry.date)
                start = entry.date
            prev = entry.date
        #Last entry always closes the activity, can't expect break..
        act.set_end(prev)
        all_activities.append(act)

        return all_activities
