#!/usr/bin/python
import sys
import filecmp
import os
import shutil

f=file(sys.argv[1])
empty_dirs=set()
rm=set()
cmds=file(sys.argv[2], "w")

for x in f.readlines():
    (b,f,src,a,dst,d)=x.split(' ')
    e_src=os.path.exists(src)
    e_dst=os.path.exists(dst)
    pdir=os.path.dirname(src)
    pdir_dst=os.path.dirname(dst)
    last=None
    while not os.path.exists(pdir):
        last=pdir
        last_dst=pdir_dst
        pdir=os.path.dirname(pdir)
        pdir_dst=os.path.dirname(pdir_dst)

    if last:
        print "empty dir %s in src" % last
        print "     %s in dst" % last_dst
        empty_dirs.add(last_dst)

    if e_src:
        pdir_dst = os.path.dirname(dst)
        if not os.path.exists(pdir_dst):
            cmds.write("svn mkdir %s\n" % pdir_dst)
            #os.system("svn mkdir %s" % pdir_dst )
            print "want to copy but dst-dir %s doesn't exist, creating it" % pdir_dst
        print "copy ", src
        cmds.write("cp %s %s\n" % (src, dst))
#        shutil.copy(src, dst)
        if not e_dst:
            print "svn add %s" % dst
            #os.system("svn add %s" % dst)
            cmds.write("svn add %s\n" % dst)
        else:
            print "%s already in svn" % dst

    elif e_dst:
        if not last:
            print "rm ", dst
            #os.system("svn rm %s" % dst)
            cmds.write("svn rm %s/\n" % dst)
        else:
            print " skipping remove %s since directory will be removed" % dst

    else:
        print "OUCH"

for x in empty_dirs:
    cmds.write("svn rm %s\n" % x)
    print "svn rm %s" % x

sys.exit(0)


import os
from os.path import join, getsize
for root, dirs, files in os.walk(sys.argv[1]):
#    for x in files:
#        print "file:", os.joinroot
    [m,mis,err]= filecmp.cmpfiles(root, os.path.join("../4.6_new3",root), files)
    if len(mis):
        print "dir:", root

    for x in err:
        a=os.path.join(root, x)
        b=os.path.join("../4.6_new3", root, x)
        print "a:", a
        print "b:", a

#    if len(mis) != 0:
#        print " mis  ", mis
#    if len(err) != 0:
#        print " err  ", err

#    print sum(getsize(join(root, name)) for name in files),
#    print "bytes in", len(files), "non-directory files"
    if '.svn' in dirs:
        dirs.remove('.svn')  # don't visit CVS directories


for root, dirs, files in os.walk(sys.argv[1]):
    [m,mis,err]= filecmp.cmpfiles(root, os.path.join("../4.6_new3",root), files)
    if len(mis):
        print "dir:", root

    for x in err:
        a=os.path.join(root, x)
        b=os.path.join("../4.6_new3", root, x)
        print "a:", a
        print "b:", a

    if '.svn' in dirs:
        dirs.remove('.svn')  # don't visit CVS directories

sys.exit(0)



comparison = filecmp.dircmp(sys.argv[1], sys.argv[2],ignore=[".svn"])
#comparison.report_full_closure()
#comparison.report()
print comparison.common
sys.exit(0)
