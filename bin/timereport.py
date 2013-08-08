#!/usr/bin/python
import sys
import argparse
import datetime

import svnlog
import report_parser

def get_days_in_week(year, week):
    days = []
    d = datetime.date(year, 1, 1)
    delta = datetime.timedelta(days=1)
    for x in range(365):
        if int(d.strftime("%V")) == week:
            days.append(d)
        d += delta
    return days

def date_str(date):
    return date.strftime("%F")


def show_report(year, week, activities, svnentries):
    print "WEEK %d" % week
    print "========="
    weeksum = 0
    for d in get_days_in_week(year, week):
        print " == %s" % d.strftime("%a %b %d")
        entries = [e for e in svnentries if date_str(e.date) == date_str(d)]
        if entries:
            print "  SVN:"
            for e in entries:
                print "   %s <%s>" % (e.date.strftime("%H:%M:%S"), e.msg)
                if not e.msg.startswith("Merge") and len(e.paths)<10:
                    for f in e.paths:
                        print "     - ", f
                else:
                    print "       (merge?)"
        else:
            print "  SVN: None"

        sumact = 0
        for (i, a) in enumerate(activities):
            if date_str(d) == date_str(a.start):
                print "  %s" % a
                sumact += a.length()
                print "   host stats:",
                for (h, cnt) in a.get_host_stats().iteritems():
                    print "%s:%d%%(%d) " % (h, 100*cnt/len(a.entries), cnt),
                print

                print "   proj stats:",
                for (prj, cnt) in a.get_proj_stats().iteritems():
                    print "%s:%d(%d%%) " % (prj, cnt, 100*cnt/len(a.entries)),
                print

                other_pwd = a.pwd_in_proj("other")
                for (pwd, cnt) in other_pwd.iteritems():
                    print "    % 3d:%s" % (cnt, pwd)

                if i+1 < len(activities):
                    print "   break %0.2fh" % (a.time_between(activities[i+1])/3600.)
        print "  sum: %0.2fh" % (sumact/3600.)

        weeksum += sumact
        print
    print "WEEKSUM: %0.2fh" % (weeksum/3600.)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Create timereport.')
    parser.add_argument('-v', '--verbose', dest='verbose',
                action='store_true', help='Print all entries in pslog.')

    parser.add_argument("--psfile", dest = "psfile", action = 'append', nargs="?", help="File containing pslogs.")
    parser.add_argument("--svnfile", dest = "svnfile", nargs="?", help="File containing svn xml log.", default = None)
    parser.add_argument("--week", dest = "week", help="Which week to produce log for.", type = int)
    parser.add_argument("--break-time", dest = "break_time", help="How long is a break.", default=3600., type = float)

    args = parser.parse_args()

    if args.svnfile != None:
        f=open(args.svnfile, "r")
        svn=svnlog.MyHTMLParser()
        for line in f:
            svn.feed(line)
        svnentries = svn.entries
    else:
        svnentries = []

    psact=report_parser.ShellActivityParser(args.break_time)
    for psfile in args.psfile:
        psact.add_entries(psfile)
#        report_parser.ShellActivityParser.convert_old(args.psfile)
#        sys.exit(0)
#        proj_act = report_parser.get_project_activities(entries, )
    activities = psact.get_activities()

    show_report(2013,
                args.week,
                activities = activities,
                svnentries = svnentries)
