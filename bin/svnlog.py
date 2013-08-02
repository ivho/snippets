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
class MyHTMLParser(HTMLParser):
    date=None
    user="error"
    msg="error"
    last="error"
    lv=-1
    ld=-1
    highscore={}
    def handle_data(self, data):
        self.last=data
    def handle_starttag(self, tag, x):
        if tag == "logentry":
            self.files = []

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
                  print "Vecka %d (%s)" % (v , self.date.strftime("%Y"))
            self.lv=v

            d=int(self.date.strftime("%d"))
            if self.ld != d:
                  print self.date.strftime("%A:")
            self.ld=d
            print "  %s %s %s" % (self.date.strftime("%H:%M"), self.msg, self.date.strftime("(%d/%b)"))
#            print (self.__dict__)
            if True:
                is_merge = self.msg.startswith("Merged") or self.msg.startswith("Blocked")
                if not is_merge:
                    for x in self.files:
                        print " - ", x
                else:
                    print " - No files shown for merge-like commits.";



if len(sys.argv) == 1:
    f=sys.stdin
else:
    f=open(sys.argv[1])

apa=MyHTMLParser()
for line in f:
    apa.feed(line)

#for x in apa.highscore:
#    print "%s = %s Svensson <%s@windriver.com>" % (x,x,x)
#    print "%d = %s" % (apa.highscore[x], x)
       
