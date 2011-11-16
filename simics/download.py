#!/usr/bin/python

import pycurl
import os

LATEST_44="https://www.simics.net/pub/simics/4.4_asp861/latest/linux64/"
DEST="/space/work/simics/download/"
UNPACK="/space/work/simics/unpack/"

last=0
def progress(download_t, download_d, upload_t, upload_d):
    global last
    nlast=download_d/(1024*1024)
    if nlast != last:
        last=nlast
        print "%dMb/%dMb" % (download_d/(1024*1024), download_t/(1024*1024))

def download(pkg):
    c = pycurl.Curl()
    name="simics-pkg-%s-4.4-linux64.tar" % pkg
    fp=os.path.join(DEST, name)
    f=open(fp, "wb")
    print f
    c.setopt(pycurl.WRITEDATA, f)
    c.setopt(c.URL, LATEST_44+name)
    c.setopt(c.NOPROGRESS, 0)
    c.setopt(c.PROGRESSFUNCTION, progress)
    print "download %s" % name
    c.perform()
    f.close()
    c.close()
    return fp

if __name__ == "__main__":
    for pkg in ["1000", "1010", "1012"]:
        fp = download(pkg)
        print "Unpacking %s" % fp
        os.system("tar -C %s -x -f %s" % (UNPACK, fp))

