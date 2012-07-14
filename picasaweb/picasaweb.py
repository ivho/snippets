#!/usr/bin/python
"""upicasa.py -- an upload script for Picasa

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
# Copied upicasa.py by Albertas Agejevas and renamed to picasaweb.
#

import os
import sys
import getopt
import getpass
import ConfigParser
import gdata.photos.service
import gdata.media
import gdata.geo


def usage():
    print "\n".join(__doc__.split("\n", 8)[:-1])
    print
    print "Usage:"
    print
    print "    upicasa.py -l|-h|-a album|-n album ..."
    print "        -n <album>   -- create new album, add photos to it"
    print "        -a <album>   -- add photos to an existing album"
    print "        -l           -- print a list of albums"
    print "        -h           -- this help"


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


def list_albums():
    """Log in, print the list of albums"""
    client = login()
    albums = client.GetUserFeed(user=client.email)
    for album in albums.entry:
        print "%s (%s photos)" % (album.title.text, album.numphotos.text)


def main():
    options, args = getopt.getopt(sys.argv[1:], "n:a:hl")

    if not options:
        return usage()

    for option, value in options:
        if option == "-n":
            new_album = True
            album_name = value
        elif option == "-a":
            new_album = False
            album_name = value
        elif option == "-h":
            return usage()
        elif option == "-l":
            return list_albums()


    gd_client = login()

    if new_album:
        album = gd_client.InsertAlbum(title=album_name, summary="")
        album_id = album.gphoto_id.text
    else:
        matches = []
        albums = gd_client.GetUserFeed(user=gd_client.email)
        for album in albums.entry:
            if album_name in album.title.text:
                matches.append((album.title.text, album.gphoto_id.text))
        if len(matches) > 1:
            print "More than 1 album matched:"
            for aname, aid in matches:
                print " * %s" % aname
            return
        elif not matches:
            print "No albums matched."
            return
        else:
            album_id = matches[0][1]
            print 'Selected album "%s"' % matches[0][0]

    album_url = '/data/feed/api/user/%s/albumid/%s' % (
        gd_client.email, album_id)
    for photo in args:
        print "Uploading", photo
        photo = gd_client.InsertPhotoSimple(album_url, 'New Photo',
                                            '', # title
                                            photo, content_type='image/jpeg')


if __name__ == '__main__':
    main()
