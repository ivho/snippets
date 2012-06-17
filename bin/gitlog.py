#!/usr/bin/python
import sys
import datetime
import os
from time import strptime
from email.utils import parsedate_tz
#print parsedate_tz('Fri, 15 May 2009 17:58:28 +0700')

if __name__ == "__main__":
    os.system("rm /tmp/timerep.ivho")
    for proj in "qsp", "u-boot", "linux_qsp", "vxworks_qsp", "qsp", "Simics":
        os.system("cd /space/work/simics/qsp/%s" % proj)
        os.system("git log --date=short --format=format:'%%at|%%an|%s|%%s'  --date=rfc --since=2012-01 >> /tmp/timerep.ivho" %
                  proj)
        os.system("echo >> /tmp/timerep.ivho")

    f=file("/tmp/timerep.ivho")
    lastweek = -1
    ld=-1
    while True:
        xx=f.readline()
        if len(xx) == 0:
            break
#        print xx
        (sdate,author,proj,msg)=xx.split('|')

#        print sdate, author, msg
#        sdate=x[0]
        try:
#            datetime.datetime(*strptime(sdate,"%Y-%m-%d")[0:6])
            date = datetime.datetime.utcfromtimestamp(int(sdate))
#            date=parsedate_tz(sdate)
            w=int(date.strftime("%V"))
            if w != lastweek:
                print "Vecka %d" % w
            lastweek=w

            d=int(date.strftime("%d"))
            if ld != d:
                  print date.strftime("%A %d/%b:")
            ld=d
            print "  %s <%s> %s" % (date.strftime("%H:%M"), proj, msg.strip())

        except IndexError:
            break
