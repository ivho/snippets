#!/usr/bin/python

from scapy.all import *

x=Ether(src="ff:ff:ff:ff:ff:ff",dst="11:22:33:44:55")/IP(dst="10.10.0.1",src="10.10.55.1")/UDP()/"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
print map(ord, x.build())
