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
        self.epoch = int(date.strftime("%s"))
        self.proj = pwd_to_proj(pwd)

    def __repr__(self):
        return "%s %s %s %s" % (self.date.__str__(), self.host, self.pwd, self.cmd.strip())

    def new_style(self):
        return "1:%s:%s:%s:%s" % (self.epoch,
                                  self.host,
                                  self.pwd,
                                  self.cmd.strip())

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

    def time_between(self, next):
        return (time.mktime(next.start.timetuple()) -
                time.mktime(self.end.timetuple()))

    def __repr__(self):
        return "%0.2fh %s -> %s (%d entries)" % (
            self.length()/3600.,
            self.start.strftime("%H:%M"),
            self.end.strftime("%H:%M"),
            len(self.entries))

    def get_proj_stats(self):
        ret = {}
        for e in self.entries:
            if not e.proj in ret:
                ret[e.proj] = 1
            else:
                ret[e.proj] += 1
        return ret

    def get_host_stats(self):
        ret = {}
        for e in self.entries:
            if not e.host in ret:
                ret[e.host] = 1
            else:
                ret[e.host] += 1
        return ret

    def pwd_in_proj(self, proj):
        ret = {}
        for e in self.entries:
            if e.proj == proj:
                if not e.pwd in ret:
                    ret[e.pwd] = 1
                else:
                    ret[e.pwd] += 1
        return ret

    def week(self):
        return int(self.start.strftime("%V"))

def get_epoch(date):
    return time.mktime(date.timetuple())

class ShellActivityParser(object):
    def __init__(self, break_time):
        self.break_time = break_time
        self.entries = []

    def add_entries(self, filename):
        skipped = 0
        print "Add %s" % filename
        f = file(filename, "r")
        for l in f.readlines():
            e = self.parse_line(l)
            if e != None:
                self.entries.append(e)
            else:
                skipped += 1
        self.entries.sort(key = lambda a:a.epoch)
        print len(self.entries)
        print "Skipped %d entries for %s" % (skipped, filename)

    @staticmethod
    def convert_old(fn):
        def parse_date(datestr):
            date = datetime.datetime.strptime(datestr, '%a %b %d %H:%M:%S %Z %Y ')
            return date

        for l in file(fn,"r").readlines():
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
                e = Entry(date = parse_date(datestr),
                          host = host,
                          pwd = pwd,
                          cmd = cmd)
                print e.new_style()

    def parse_line(self, l):
        try:
            if l.strip() == "":
                return None
            rev = int(l.split(":", 1)[0])
            #rev 1, rev:epoch:hostname:pwd:cmd
            if rev == 1:
                parts = l.split(":", 5)
                date = datetime.datetime.fromtimestamp(int(parts[1]))
                entry = Entry(date = date,
                              host = parts[2],
                              pwd = parts[3],
                              cmd = parts[4])
                return entry
        except IndexError:
            raise Exception("Unable to parse <%s>" % l.strip())
        return None


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
