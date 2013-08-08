#!/usr/bin/python
import sys
import argparse
import datetime

import svnlog
import report_parser
import gitlog

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


def show_report(year, week, activities, svnentries, verbose, gitcommits):
    print "WEEK %d" % week
    print "========="
    weeksum = 0
    for d in get_days_in_week(year, week):
        print " == %s" % d.strftime("%a %b %d")
        entries = [e for e in svnentries if date_str(e.date) == date_str(d)]
        if entries:
            print "  SVN:"
            for e in entries:
                print "   %s r%d <%s>" % (e.date.strftime("%H:%M:%S"), e.rev, e.msg)
                if verbose > 1:
                    if not e.msg.startswith("Merge") and len(e.paths)<10:
                        for f in e.paths:
                            print "     - ", f
                    else:
                        print "       (merge?)"
        else:
            print "  SVN: None"

        commits = [e for e in gitcommits
                   if date_str(e.adate) == date_str(d)]
        commits.sort(key = lambda a:a.adate)
        if len(commits) != 0:
            print "  GIT:"
            for commit in commits:
                print "   %s %s <%s>" % (commit.adate.strftime("%H:%M:%S"),
                                                commit.sha[:8],
                                                commit.msg.strip())
                if verbose > 1:
                    for f in commit.get_files():
                        print "     - ", f
        else:
            print "  GIT: None"

        sumact = 0
        if len(activities) != 0:
            print "  SHELL:"
        else:
            print "  SHELL: None"
        for (i, a) in enumerate(activities):
            if date_str(d) == date_str(a.start):
                if i+1 < len(activities):
                    br = "break %0.2fh" % (a.time_between(activities[i+1])/3600.)
                else:
                    br = "last entry"
                print "  ** %s (%s)" % (a, br)

                sumact += a.length()
                if verbose > 0:
                    print "   host stats:",
                    for (h, cnt) in a.get_host_stats():
                        print "%s:%d%%(%d) " % (h, 100*cnt/len(a.entries), cnt),
                    print

                    print "   proj stats:",
                    for (prj, cnt) in a.get_proj_stats():
                        print "%s:%d(%d%%) " % (prj, cnt, 100*cnt/len(a.entries)),
                    print

                if verbose > 1:
                    projs = [p[0] for p in a.get_proj_stats()]
                    for prj in projs:
                        other_pwd = a.pwd_in_proj(prj)
                        print "    PWD:", prj
                        for (pwd, cnt) in other_pwd:
                            print "    % 3d:%s" % (cnt, pwd)

                if verbose > 2:
                    for e in a.entries:
                        print "    ", e
        print "  sum: %0.2fh" % (sumact/3600.)

        weeksum += sumact
        print
    print "WEEKSUM: %0.2fh" % (weeksum/3600.)

class FlatAppend(argparse.Action):
    def __call__(self, parser, namespace, values, option_string=None):
        setattr(namespace, self.dest,  getattr(namespace, self.dest) + values)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Create timereport.')
    parser.add_argument('-v', '--verbose',
                        dest='verbose',
                        action='count',
                        help='Print all entries in pslog.')

    parser.add_argument("-p", "--psfile",
                        dest = "psfile",
                        action = FlatAppend,
                        nargs='+',
                        help="File containing pslogs.",
                        default = [])

    parser.add_argument("-g", "--git-dir",
                        dest = "gitdirs",
                        action = FlatAppend,
                        nargs='+',
                        help="Directory containing a git repo (not bare).",
                        default = [])

    parser.add_argument("--git-all",
                        dest = "gitall",
                        help="Collect git entries with --all option to log.",
                        action = "store_true",
                        default = False)

    parser.add_argument("-s", "--svnfile",
                        dest = "svnfile",
                        nargs=1,
                        help="File containing svn xml log.",
                        default = None)


    parser.add_argument("--week",
                        nargs = '+',
                        dest = "week",
                        type = int,
                        required = True,
                        action = "store",
                        help="Which weeks to produce reports for.")

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

    commits = []
    for gitdir in args.gitdirs:
        ga = gitlog.GitActivity(gitdir, args.gitall)
        commits += ga.commits

    for week in args.week:
        show_report(2013,
                    week,
                    activities = activities,
                    svnentries = svnentries,
                    verbose = args.verbose,
                    gitcommits = commits)
