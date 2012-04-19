#!/usr/bin/python

import os
import sys

def solve(inf, outf):
    firstline=inf.readline().split()
    nocases=int(firstline[0])

    for lineno in xrange(nocases):
        seen={}
        linestr=inf.readline().split()
        sublen=int(linestr[0])
        tstr=linestr[1]
        print "STR:", tstr
        print "LEN:", sublen
        outf.write("Case #%d: " % (lineno + 1))
        for x in xrange(len(tstr)-sublen+1):
            subs=tstr[x:x+sublen]
            try:
                seen[subs]+=1
            except KeyError:
                seen[subs]=1
        nl=[]
        for a in seen:
            if seen[a]!=1:
                print "a:", a
                nl.append(int(a))

        if len(nl)==0:
            outf.write("NONE")
        else:
            for x in sorted(nl):
                outf.write("%d " % x)
        outf.write("\n")
if __name__ == "__main__":
    try:
        out=file(sys.argv[1], "w")
    except IndexError:
        out=sys.stdout

    solve(sys.stdin, out)
