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

# TODO: handle internal server error exceptions on create_album and inserttags as well.


import sys
import os
import hashlib
import time
import argparse
import string
import shutil
import urllib
import urllib2
import calendar

from PIL import Image
from PIL.ExifTags import TAGS

import ConfigParser

try:
    import gdata.photos.service
    import gdata.media
    import gdata.geo
except ImportError as e:
    print "Unable to import gdata libraries."
    print " on ubuntu try:"
    print ""
    print "sudo apt-get install python-gdata"
    sys.exit(1)


HASH_BYTES=2*1024
MIN_SIZE=32*1024

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
        self.tot_size = 0
        self.skipped = 0
        self.no_exif = 0
        self.failed_to_open = 0
        self.all_tags = set()
        self.tag_count = {}
        self.too_small = 0
        if name:
            self.default_album_prefix = name
        else:
            self.default_album_prefix = ""

        if sync_to_picasa:
            self.picasa = self.Picasa()
        else:
            self.picasa = None


    def add_photos(self):
        for root, dirs, files in os.walk(self.root):
            prefix=self.default_album_prefix
            d=root

            while d.rstrip("/") != self.root.rstrip("/") and d != "/":
                try:
                    prefix=file(os.path.join(d, "album_prefix"), "r").read().strip()
                    break
                except:
                    pass
                d=os.path.dirname(d)
            if len(files) == 0:
                dprint("empty dir % s" % root)
                continue
#            for f in [files[0]]:
            for f in files:
                fp=os.path.join(root, f)
                if is_photo(fp):
                    bdir=None
                    if self.backup:
                        bdir = string.replace(root, self.root, os.path.join(self.backup.root, self.name, ""))
                    #ignore broken links...
                    if os.path.exists(fp):
                        dprint("checking %s prefix<%s>" % (fp, prefix))
                        if prefix != "":
                            xprefix = prefix+"-"
                        else:
                            xprefix = prefix
                        self.handle_photo(fp, root, bdir, prefix = xprefix)
                else:
                    dprint("%s not a photo" % fp)

    class Picasa():
        def __init__(self):
            global p
            self.client = None
            self.albums = None
            self.photos = {}
            self.album_check = {}
            self.fetch_picasa()

        def download_photo(self, photo):
            "Download the data at URL to the current directory."
            fname=self.get_filename(photo)
            print "Downloading %s" % (fname)
            urllib.urlretrieve(url, "dl/" + self.fname)

        def unquote_recurse(self, url, max=10):
            u=urllib2.unquote(url)
            if u == url or max == 0:
                return u
            return self.unquote_recurse(u, max-1)

        def get_filename(self, photo):
            url=self.unquote_recurse(photo.content.src)
            fname=url[url.rindex('/') + 1:]
            return fname

        def fetch_picasa(self):
            """Fetch albums and photo info from picasa account."""
            global p

            self.client=login()
            self.albums = self.client.GetUserFeed(user=self.client.email)
            for album in self.albums.entry:
                print "%s (%s photos)" % (album.title.text, album.numphotos.text)
                self.album_check[album.title.text] = album
                album.photos = self.client.GetFeed(
                    '/data/feed/api/user/%s/albumid/%s?kind=photo' % (
                        self.client.email, album.gphoto_id.text))
                if p.delete_picasa:
                    if self.client.email != 'ivartestimage@gmail.com':
                        print "Refusing to delete on <%s>" % self.client.email
                        sys.exit(1)
                    print "deleting album"
                    self.client.Delete(album)
                    continue

                for photo in album.photos.entry:
                    # This looks like a picasa timezone bug
                    # subtracting 2 hours since we're in Sweden...
                    try:
                        dummy=photo.exif.time.text
                    except AttributeError:
                        print "Failed to get exif.time for %s (url:%s)" % (self.get_filename(photo),  photo.content.src)
                        continue

                    print "picasa <%s> - %s (org:%s)"  % (self.get_filename(photo).lower(), time.ctime(int(photo.exif.time.text)/1000), photo.exif.time.text)
                    key="%s %d" % (self.get_filename(photo) .lower(), (int(photo.exif.time.text)/1000)) # -3600 removed
                    self.photos[key] = photo
                    dprint("added key:<%s>  (org time %s) to dict unique:%s src:%s " % (key, photo.exif.time.text, photo.exif.imageUniqueID.text, photo.content.src))
#                    print photo
#                    dprint("w:%d h:%d s:%d" % (photo.content.width,
#                                               photo.content.height,
#                                               photo.content.fileSize))
#                    self.download_file(photo.content.src)


        def add_to_picasa(self, ctime, fp, tags, prefix):
            global p
            album=None
            album_url=None
            # using calendat.timegm instead time.mktime since picasa will return TZ adjusted times...
            key = "%s %d" % (os.path.basename(fp).lower(), int(calendar.timegm(ctime)))
            album_name = prefix+time.strftime("%Y-%b",ctime)
            if self.photos.has_key(key):
                dprint("%s already in picasa" % key)
                return
            else:
                print "KEY:<%s> not in picasa .photos" % key

            if not self.album_check.has_key(album_name):
                dprint("Creating album %s" % album_name)
                if not p.dry_run:
                    album_date=calendar.timegm(ctime)*1000 # picasa want's epoc milliseconds
                    album = self.client.InsertAlbum(title=album_name, summary="Created by photorg.py script", access='private', timestamp=str(album_date))
                    self.album_check[album_name] = album
                    album_url = '/data/feed/api/user/%s/albumid/%s' % (self.client.email, album.gphoto_id.text)
                else:
                    album_url = 'dummy'
                    album=None
            else:
                album = self.album_check[album_name]
                album_url = '/data/feed/api/user/%s/albumid/%s' % (self.client.email, album.gphoto_id.text)

            dprint("Inserting photo: <%s> key:<%s> on url %s" % (album_name, key, album_url))

            if not p.dry_run:
                print("Uploading photo %s to album %s" % (os.path.basename(fp), album_name))
                sys.stdout.flush()
                attempt=0
                while True:
                    try:

#                        if attempt < 3:
#                            print "Causing internal error"
#                            raise gdata.photos.service.GooglePhotosException({"status":500, "reason":'Internal Server Error',"body":"Unknown"})
                        photo = self.client.InsertPhotoSimple(album_url, os.path.basename(fp),
                                                              '', # title
                                                              fp, content_type='image/jpeg')
                    except gdata.photos.service.GooglePhotosException as (status, reason,body):
                        if status==500 and reason=='Internal Server Error':
                            print "%d: Got <%s %s %s> while instering photo, retrying in 1 sec." % (attempt, status, reason, body)
                            time.sleep(1)
                            attempt += 1
                            continue
                        raise

                    self.photos[key]=photo
                    photo_url = '/data/feed/api/user/%s/albumid/%s/photoid/%s' % (self.client.email, album.gphoto_id.text, photo.gphoto_id.text)
                    for tag in tags:
                        dprint("inserting tag %s" % tag)
                        tag = self.client.InsertTag(photo_url, tag)
                    break
            else:
                self.photos[key]="dummy"
                dprint("skipping - dry-run.")


    def tag_filter(self, d):
        black_list=[
            "cs1",
            "old_gamepc",
            "proj: 160",
            "src.slask",
            "100olymp",
            "root",
            "100_____",
            "$Desktop",
            "[C]",
            "101MSDCF",
            "102MSDCF",
            "lib",
            "var",
            "photos",
            "unknown_disk",
            "$My Pictures",
            "AnnaBritta",
            "backups",
            "wd_passport",
            "45AA-0E52",
            "All_Photos",
            "DCIM",
            "dcim"]
        for x in black_list:
            if x == d:
                return False

        d.replace('olymp','')
        try:
            x=int(d)
            return False
        except ValueError:
            if len(d) == 0:
                return False
            return True

    def create_tags(self, fp):
        dname=os.path.dirname(fp)
        dprint("fp:%s root:%s %s" % (fp, self.root, dname))
        if not dname.startswith(self.root):
            print "Ouch"
            sys.exit(1)
            return
        #picasa tags are not allowed to contain ","
        dname=dname.replace(",","_")
        dirs=dname[len(self.root):].split('/')
        return filter(self.tag_filter, dirs)

    def handle_photo(self, fp, root, bdir, prefix):
#        print "%s" % fp
        dprint("====HANDLE==== %s (prefix:%s)" % (fp, prefix))

        if os.path.getsize(fp) < MIN_SIZE:
            print "Too small:", fp
            self.too_small += 1
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
            exif={}
            try:
                exif=get_exif(fp)
            except AttributeError:
#                print("Failed to parse exif for %s" % fp)
                self.no_exif += 1
                return

            try:
                dprint("uniq:%s" % exif['imageUniqueID'])
            except KeyError:
                dprint("no imageUniqueID found.")


            # picasa only uses DateTimeOriginal when returning photo.exif.time
            # so if it's not there, we _REALLY_ should upload it since we can't detect duplicates
            # for it.
            # Try something like:
            # $ exiftool '-CreateDate<ModifyDate' bad_time.jpg
            #
            try:
                c = time.strptime(exif['DateTimeOriginal'], "%Y:%m:%d %H:%M:%S")
            except KeyError:
                print("Failed to find date %s" % fp)
                self.skipped += 1
                return
#                c=time.localtime()
            except ValueError:
                print("==== Failed to parse date %s <%s>" % (fp, exif["DateTimeOriginal"]))
                self.skipped += 1
                for a in exif:
                    try:
                        if "date" in a.lower() or "time" in a.lower():
                            dprint("found tag <%s> -> <%s>" % (a,exif[a]))
                            try:
                                dprint("exif:%s" % exif[a])
                                xxx = time.strptime(exif[a], "%Y:%m:%d %H:%M:%S")
                                print("Could use %s -> %s " % ( a, exif[a]))
                                # but we don't, see comment above. We would need to
                                # modify the original and I don't want that here, should
                                # go in separate script.
                            except ValueError:
                                dprint("no parse <%s>" % exif[a])
                                pass
                            except TypeError:
                                dprint("no parse <%s>" % a)
                    except AttributeError:
                        pass
                return

            try:
                make = exif['Make']
            except KeyError:
                make = 'unknown'

            tags = self.create_tags(fp)
            for t in tags:
                self.all_tags.add(t)
                try:
                    self.tag_count[t] += 1
                except KeyError:
                    self.tag_count[t] = 1

            if self.picasa:
                dprint("tags: %s" % tags)
                self.picasa.add_to_picasa(c, fp, tags, prefix)

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
                self.tot_size += os.path.getsize(fp)
                if not root in self.dirs:
                    self.dirs[root] = 1
                else:
                    self.dirs[root] += 1
            else:
                global doubles
                (ofp, oc, make)=self.photos[hashstr]

                dprint("double %s (%s)" % (fp, ofp))
                self.doubles += 1

        except IOError:
            print("Failed to open %s" % fp)
            self.failed_to_open += 1

def usage():
    print "usage: ..."
    sys.exit(1)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-b", "--backup-dir", nargs=1)
    parser.add_argument("-n", "--name", nargs=1,
                        help="Name of backup.")
    parser.add_argument("-d", "--dry-run", action="store_true")
    parser.add_argument("-v", "--verbose", action="store_true")
    parser.add_argument("-p", "--picasa", action="store_true",
                        help="Sync with picasa, upload photos from source-dir to picasa.")
    parser.add_argument("-x", "--delete-picasa",
                        action="store_true",
                        help="Delete all photos and albums from picasa.")
    parser.add_argument("source_dir")

    global p
    p=parser.parse_args()

    if not p.source_dir:
        print "usage:"
        print parser.format_help()
        sys.exit(1)
    print p.source_dir

    b=None
    if p.backup_dir:
        print "Scanning backup: %s" % p.backup_dir[0]
        b=Collection(p.backup_dir[0])
        b.add_photos()
        print "done."

    print "Scanning source: %s" % p.source_dir
    try:
        name=p.name[0]
    except TypeError:
        name=None

    s=Collection(p.source_dir, backup = b, name = name, sync_to_picasa = p.picasa)
    if p.delete_picasa:
        sys.exit(0)

    s.add_photos()
    print "done"

    for d in s.dirs:
        dprint("DIR %d %s" % (s.dirs[d], d))
    for key, value in sorted(s.tag_count.iteritems(), key=lambda (k,v): (v,k)):
        print("%s: %s" % (key, value))

    print "photos:         %d" % len(s.photos)
    print "would_copy:     %d" % s.would_copy
    print "doubles:        %d" % s.doubles
    print "already:        %d" % s.already_in_backup
    print "tot_size:       %dGB" % (s.tot_size/(1024*1024*1024))
    print "skipped:        %d" % s.skipped
    print "no exif:        %d" % s.no_exif
    print "too small:      %d" % s.too_small
    print "failed to open: %d" % s.failed_to_open
    print "tags            %d" % len(s.all_tags)

#    print "backup tot_photos: %d" % b.tot_photos

