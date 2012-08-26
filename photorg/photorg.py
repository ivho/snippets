#!/usr/bin/python
"""

  Copyright (C) 2010 Albertas Agejevas <alga@pov.lt>
  Copyright (C) 2012 Ivar Holmqvist <ivarholmqvist@gmail.com>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""

#
# gdata authentication code "borrowed" from upicasa.py by Albertas Agejevas
#

import sys
import os
import hashlib
import time
import argparse
import string
import shutil

from PIL import Image
from PIL.ExifTags import TAGS

import ConfigParser
import gdata.photos.service
import gdata.media
import gdata.geo


HASH_BYTES=64*1024
MIN_SIZE=256*1024

def dprint(x):
    global p
    if p.verbose:
        print x

def is_photo(fname):
    lcase=fname.lower()
    for x in ['.jpg', '.jpeg', '.png']:
        if lcase.endswith(x):
            return True
    return False


def get_auth():
    """Get authentication credentials.

    If the ~/.config/upicasa/auth file exists, read the credentials
    from it, otherwise prompt for them interactively.
    """
    config_home = os.getenv("$XDG_CONFIG_HOME",
                            os.path.join(os.getenv("HOME"), ".config"))
    config_dir = os.path.join(config_home, "upicasa")
    password_path = os.path.join(config_dir, "auth")

    if not os.path.exists(password_path):
        email = raw_input("Google login: ")
        password = getpass.getpass("Password: ")
        ans = raw_input("Remember this password [Y/n]? ")

        if not ans or ans[0] not in "Nn":
            config = ConfigParser.RawConfigParser()
            config.add_section("google")
            config.set("google", "login", email)
            config.set("google", "password", password)

            if not os.path.isdir(config_dir):
                os.makedirs(config_dir)
            password_file = open(password_path, "w")
            os.fchmod(password_file.fileno(), 0700)
            config.write(password_file)
    else:
        config = ConfigParser.RawConfigParser()
        config.read(password_path)
        email = config.get("google", "login")
        password = config.get("google", "password")
    return email, password

def login():
    """Log into PicasaWeb.

    Returns a PhotosService client instance.
    """
    username, password = get_auth()
    client = gdata.photos.service.PhotosService()
    client.email = username
    client.password = password
    client.source = 'alga-upicasa-1'
    print "Authenticating..."
    client.ProgrammaticLogin()
    return client


def get_exif(fp):
    ret = {}
    i = Image.open(fp)
    info = i._getexif()
    for tag, value in info.items():
        decoded = TAGS.get(tag, tag)
        ret[decoded] = value
    return ret

class Collection(object):
    def __init__(self, root_dir, name = None, backup = None, sync_to_picasa = False):
        self.dirs = {}
        self.root = root_dir
        self.photos = {}
        self.doubles = 0
        self.backup = backup
        self.already_in_backup = 0
        self.name = name
        self.tot_photos = 0
        self.would_copy = 0
        if sync_to_picasa:
            self.picasa = self.Picasa()
        else:
            self.picasa = None

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
                else:
                    dprint("%s not a photo" % fname)

    class Picasa():
        def __init__(self):
            self.client = None
            self.albums = None
            self.photos = {}
            self.album_check = {}
            self.fetch_picasa()

        def fetch_picasa(self):
            """Fetch albums and photo info from picasa account."""
            self.client=login()
            self.albums = self.client.GetUserFeed(user=self.client.email)
            for album in self.albums.entry:
                print "%s (%s photos)" % (album.title.text, album.numphotos.text)
                self.album_check[album.title.text] = album
                album.photos = self.client.GetFeed(
                    '/data/feed/api/user/%s/albumid/%s?kind=photo' % (
                        self.client.email, album.gphoto_id.text))

                for photo in album.photos.entry:
                    key="%s %s" % (photo.title.text, photo.timestamp.text)
                    self.photos[key] = photo
                    dprint("added key:<%s> to dict unique:%s" % (key, photo.exif.imageUniqueID.text))


        def add_to_picasa(self, ctime, fp):
            print ctime
            key = "%s %d" % (os.path.basename(fp), int(time.mktime(ctime)))
            album_name = "test-%d-%02d-%02d" % (ctime.tm_year,ctime.tm_mon,ctime.tm_mday)
            if self.photos.has_key(key):
                dprint("%s already in picasa" % key)
                return

            if not self.album_check.has_key(album_name):
                dprint("Creating album %s" % album_name)
                self.album_check[album_name] = self.client.InsertAlbum(title=album_name, summary="Created by photorg.py script")
            else:
                dprint("%s already exists, reusing" % album_name)
            album=self.album_check[album_name]
            album_url = '/data/feed/api/user/%s/albumid/%s' % (self.client.email, album.gphoto_id.text)

            dprint("Inserting photo: <%s> key:<%s> on url %s" % (album_name, key, album_url))
#            photo = self.client.InsertPhotoSimple(album_url, os.path.basename(fp),
#                                                '', # title
#                                                fp, content_type='image/jpeg')


    def handle_photo(self, fp, root, bdir):
        dprint("====HANDLE==== %s" % fp)
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
            print "uniq:%s" % exif['imageUniqueID']
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
#            if self.sync_with_picasa:
            if self.picasa:
                self.picasa.add_to_picasa(c, fp)

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
        except None:
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
    parser.add_argument("-p", "--picasa", action="store_true")
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
    s=Collection(p.source_dir[0], backup = b, name = p.name[0], sync_to_picasa = p.picasa)

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

