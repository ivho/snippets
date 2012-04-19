#!/usr/bin/python

import os
import sys

def solve(inf, outf):
    firstline=inf.readline().split()
    nocase=int(firsline[0])

    for lineno in nocases:

        linestr=inf.readline().split()
        lineint=map(int, linestr)
        outf.write("Case #%d: " % lineno)

        for val in linea:
            outf.write("0x%08x " % val)

        outf.write("\n")

if __name__ == "__main__":
    try:
        out=file(sys.argv[1], "w")
    except IndexError:
        out=sys.stdout

    solve(sys.stdin, out)
