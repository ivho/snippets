#!/usr/bin/python
from math import pow
from math import sqrt
from math import ceil
b=12
r=60

def x(n):
    return b*(n+0.5)

def l(n):
    return 2*sqrt(pow(r,2)-pow(x(n-1), 2))

sum=ceil(2*r)
print "l[0] = 1 x %d" % sum
for n in xrange(1,128):
    try:
        ln=ceil(l(n))
        print "l[%d] = 2 x %f" % (n, ln)
        sum+=2*ln
    except ValueError:
        break

print "sum=%d" % sum
