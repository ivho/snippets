#!/usr/bin/python
import sys
import os
import fileinput
import datetime

from time import strptime
from HTMLParser import HTMLParser

user=os.getenv("USER")
#user="rdickson"
user="iholmqvi"
#users=["vsharma", "gkhanna", "mbarthol"]
users=["iholmqvi"]
print

class SvnEntry(object):
    def __init__(self, date, author, paths, msg, rev):
        self.msg = msg
        self.date = date
        self.author = author
        self.paths = paths
        self.msg = msg
        self.rev = rev

    def __repr__(self):
        return "r%d %s %s %s" % (self.rev, self.author, self.date, self.msg)

def dprint(x):
    if __name__ == "__main__":
        print x

class MyHTMLParser(HTMLParser):
    date=None
    user="error"
    msg="error"
    last="error"
    lv=-1
    ld=-1
    highscore={}
    entries = []
    def handle_data(self, data):
        self.last=data
    def handle_starttag(self, tag, x):
        if tag == "logentry":
            self.files = []
            for (attr, val) in x:
                if attr == 'revision':
                    self.rev = int(val)

    def handle_endtag(self, tag):
        if tag == "date":
            a=self.last.split(".")
            self.date = datetime.datetime(*strptime(a[0],"%Y-%m-%dT%H:%M:%S")[0:6])
        if tag == "author":
            self.user=self.last
            try:
                self.highscore[self.user] += 1
            except KeyError:
                self.highscore[self.user] = 1

        if tag == "msg":
            self.msg=self.last

        match = False
        for x in users:
            if x == self.user:
                match = True

#        if tag == "path" and self.user == user:
        if tag == "path":
            self.files += [self.last]

        if tag == "logentry" and match:
            v=int(self.date.strftime("%V"))
#            if v < 47:
#                sys.exit(0)

            if self.lv != v:
                  print("Vecka %d (%s)" % (v , self.date.strftime("%Y")))
            self.lv=v

            d=int(self.date.strftime("%d"))
            if self.ld != d:
                dprint(self.date.strftime("%A:"))
            self.ld=d
            dprint("  %s %s %s" % (self.date.strftime("%H:%M"), self.msg, self.date.strftime("(%d/%b)")))
#            print (self.__dict__)
#            if True:
            if False:
                is_merge = self.msg.startswith("Merged") or self.msg.startswith("Blocked")
                if not is_merge:
                    for x in self.files:
                        dprint(" - %s" % x)
                else:
                    dprint(" - No files shown for merge-like commits.")
            entry = SvnEntry(date = self.date,
                             author = self.user,
                             msg = self.msg,
                             paths = self.files,
                             rev = self.rev)
            self.entries.append(entry)

if __name__ == "__main__":
    if len(sys.argv) == 1:
        f=sys.stdin
    else:
        f=open(sys.argv[1])

    apa=MyHTMLParser()
    for line in f:
        apa.feed(line)

# svn log --xml -v http://ala-svn.wrs.com/svn/Simics -r {2013-06-01}:HEAD
#

#for x in apa.highscore:
#    print "%s = %s Svensson <%s@windriver.com>" % (x,x,x)
#    print "%d = %s" % (apa.highscore[x], x)
       
