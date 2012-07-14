#!/usr/bin/python

import sys
import os
import hashlib
import time
import argparse
import string
import shutil

from PIL import Image
from PIL.ExifTags import TAGS

HASH_BYTES=64*1024
MIN_SIZE=256*1024

def dprint(x):
    global p
    if p.verbose:
        print x

def is_photo(fname):
    lcase=fname.lower()
    for x in ['.jpg', '.jpeg', '.png']:        
        if fname.endswith(x):
            return True
    return False

def get_exif(fp):
    ret = {}
    i = Image.open(fp)
    info = i._getexif()
    for tag, value in info.items():
        decoded = TAGS.get(tag, tag)
        ret[decoded] = value
    return ret

class Collection(object):
    def __init__(self, root_dir, name = None, backup = None):
        self.dirs = {}
        self.root = root_dir
        self.photos = {}
        self.doubles = 0
        self.backup = backup
        self.already_in_backup = 0
        self.name = name
        self.tot_photos = 0
        self.would_copy = 0

    def add_photos(self):
        for root, dirs, files in os.walk(self.root):
            for f in files:
                fp=os.path.join(root, f)
                if is_photo(fp):
                    bdir=None
                    if self.backup:
                        bdir = string.replace(root, self.root, os.path.join(self.backup.root, self.name, ""))
                    #ignore broken links...
                    if os.path.exists(fp):
                        dprint("checking %s" % fp)
                        self.handle_photo(fp, root, bdir)        

    def handle_photo(self, fp, root, bdir):
        if os.path.getsize(fp) < MIN_SIZE:
            return
        m=hashlib.md5()
        m.update(file(fp).read(HASH_BYTES))
        hashstr=m.hexdigest()
        self.tot_photos += 1
        if self.backup and hashstr in self.backup.photos:
            (ofp, oc, make)=self.backup.photos[hashstr]
            dprint("already in backup %s (%s)" % (fp, ofp))
            self.already_in_backup += 1;            
            return

        try:
            exif=get_exif(fp)
            try:
                c = time.strptime(exif['DateTime'], "%Y:%m:%d %H:%M:%S")
            except KeyError:
                dprint("Failed to find date %s" % fp)
#                return
                c=time.now()
            except ValueError:
                dprint("Failed to parse date %s" % fp)
                c=time.now()
#                return
                
            try:
                make = exif['Make']
            except KeyError:
                make = 'unknown'
#                return
            
            if not hashstr in self.photos:
                if bdir:
                    global p
                    dst=os.path.join(bdir,os.path.basename(fp))
                    if not p.dry_run:
                        print "copy %s -> %s" % (fp, dst)
                        if not os.path.exists(bdir):
                            os.makedirs(bdir)
                        shutil.copy(fp, dst)
                    else:
                        self.would_copy += 1
                        print "(dry)copy %s -> %s" % (fp, dst)
                self.photos[hashstr]=(fp, c, make)
                if not root in self.dirs:
                    self.dirs[root] = 1
                else:
                    self.dirs[root] += 1
            else:
                global doubles
                (ofp, oc, make)=self.photos[hashstr]
                
                print "double %s (%s)" % (fp, ofp)
                self.doubles += 1

        except IOError:
            dprint("Failed to open %s" % fp)
        except AttributeError:
            dprint("Failed to parse exif for %s" % fp)

def usage():
    print "usage: ..."
    sys.exit(1)    

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-b", "--backup-dir", nargs=1)
    parser.add_argument("-n", "--name", nargs=1)
    parser.add_argument("-s", "--source-dir", nargs=1)
    parser.add_argument("-d", "--dry-run", action="store_true")
    parser.add_argument("-v", "--verbose", action="store_true")
    global p
    p=parser.parse_args()

    print   p.backup_dir
    print p
    b=None
    if p.backup_dir:
        print "Scanning backup..."
        b=Collection(p.backup_dir[0])
        b.add_photos()
        print "done."
    
    print "Scanning source..."
    s=Collection(p.source_dir[0], backup = b, name = p.name[0])
    s.add_photos()
    print "done"

    for d in s.dirs:
        print "DIR %d %s" % (s.dirs[d], d)
    print "photos:     %d" % len(s.photos)
    print "tot_photos: %d" % s.tot_photos
    print "would_copy: %d" % s.would_copy
    print "doubles:    %d" % s.doubles
    print "already:    %d" % s.already_in_backup

    print "backup tot_photos: %d" % b.tot_photos

