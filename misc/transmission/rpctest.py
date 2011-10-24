#!/usr/bin/python

import jsonrpclib


#s = jsonrpclib.ServerProxy("http://jsolait.net/services/test.jsonrpc", verbose=0)

s2 = jsonrpclib.ServerProxy("http://localhost:9091/services/roicgi.py", verbose = 0)
#s2 = jsonrpclib.ServerProxy("http://localhost:8080/json", verbose = 0)
reply = s2.get_lands()
for a in reply['result']:
    print a['lid']
#reply = s2.helloworld()
#print reply
#apa(reply['result'])
import sys
sys.exit()

s2.create_land("Farm", 50, 1)
lid2=s2.create_land("Lumber", 250, 5)['result']
print "LID2: %s" % lid2
s2.update_land(lid2, 10)

for land in s2.get_lands()['result']:
    s2.update_land(land['lid'], land['owned']+1)

apa(s2.get_lands()['result'])    

