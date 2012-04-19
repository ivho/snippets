#!/usr/bin/python

import os
import sys

def solve(inf, outf):
    firstline=inf.readline().split()
    nocases=int(firstline[0])

    for lineno in xrange(nocases):

        linestr=inf.readline().split(' ')
        lineint=map(int, linestr)
        outf.write("Case #%d: " % (lineno + 1))
        L=lineint[0]
        M=lineint[1]
        K=lineint[2:]
#        print K
        print "(stock) L", L
        print "(numfig) M", M
        print "(want) K", K
        minx=0
        avail=L*M
        print "avail", avail
        print "minx", minx
        for val in K:
            x=L*(M-1)+val
            print "x", x
            if x>avail:
                minx=-1
                break
            minx=max(x, minx)
            print "new:", minx
#            outf.write("%d " % val)
        print "ANS:", minx
        outf.write("%d\n" % minx)


if __name__ == "__main__":
    try:
        out=file(sys.argv[1], "w")
    except IndexError:
        out=sys.stdout

    solve(sys.stdin, out)
