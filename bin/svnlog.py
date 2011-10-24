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
users=["vsharma", "gkhanna", "mbarthol"]
print
class MyHTMLParser(HTMLParser):
    date=None
    user="error"
    msg="error"
    last="error"
    lv=-1
    ld=-1
    
    def handle_data(self, data):
        self.last=data    

    def handle_endtag(self, tag):
        if tag == "date":
            a=self.last.split(".")
            self.date = datetime.datetime(*strptime(a[0],"%Y-%m-%dT%H:%M:%S")[0:6])
        if tag == "author":
            self.user=self.last
#            print "author:", self.user
        if tag == "msg":
            self.msg=self.last
        match = False
        for x in users:
            if x == self.user:
                match = True

        if tag == "logentry" and match:
            v=int(self.date.strftime("%V"))
            if self.lv != v:
                  print "Vecka %d (%s)" % (v , self.date.strftime("%Y"))
            self.lv=v

            d=int(self.date.strftime("%d"))
            if self.ld != d:
                  print self.date.strftime("%A:")
            self.ld=d
            print "  %s %s %s" % (self.date.strftime("%H:%M"), self.msg, self.date.strftime("(%d/%b)"))
#            print (self.__dict__)

#        if tag == "path" and self.user == user:
        if tag == "path":
            print "      -%s " %  self.last

if len(sys.argv) == 1:
    f=sys.stdin
else:
    f=open(sys.argv[1])

apa=MyHTMLParser()
for line in f:
    apa.feed(line)

