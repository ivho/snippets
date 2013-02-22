#!/usr/bin/env python

import sys, optparse
import subprocess
import sha
import os
import shutil
import stat

PLACEHOLDER_SUFFIX = ".simimg"

def log(msg):
    print "=== ", msg

def svn_cmd(cmd):
    log(" === svn command <%s>" % cmd)
    r = subprocess.call(cmd, shell = True)
    if r != 0:
        log("svn cmd <%s> failed" % cmd)
    return r

def svn_add_file(path):
    return svn_cmd("svn add %s" % path)

def svn_mkdir(dirname):
    return svn_cmd("svn mkdir %s" % dirname)
    r = subprocess.call("svn mkdir %s" % dirname)

def svn_commit(msg, paths):
    if type(paths) == type([]):
        args = " ".join(paths)
    else:
        args = paths
    cmd = "svn commit -m \"%s\" --depth empty %s" % (msg, args)
#    print "skipping commit cmd <%s>" % cmd
    return svn_cmd(cmd)

def get_hash(path):
    log("get hash for <%s>" % path)
    s = sha.new()
    s.update(file(path).read())
    return s.hexdigest()

def create_img(o, path, src_img_path = None):

    replace = False
    if not src_img_path:
        replace = True
        src_img_path = path

    # Get the hash of the file used for archiving
    # and split it into git-like structure.
    # xx/yyyyyyyyyyy
    hash = get_hash(src_img_path)
    dirname = hash[:2]
    fname = hash[2:]

    # Create the file in img repo with hash based name
    img_dir = os.path.join(o.repo, dirname)
    img_path = os.path.join(img_dir, fname)
    svn_mkdir(img_dir)
    shutil.move(path, img_path)
    svn_add_file(img_path)

    # Create the placeholder file containg the hash
    placeholder_path = path+PLACEHOLDER_SUFFIX
    hash_placeholder = file(placeholder_path, "w")
    hash_placeholder.write(hash)
    hash_placeholder.close()
    svn_add_file(placeholder_path)

    # Create the local symlink pointing to the img_repo version
    # FIXME: Need a WIN32 version of this!!!!
    if replace:
        shutil.remove(path)

    os.symlink(img_path, path)
    local_img_dir = os.path.dirname(path)

    # Add the symlink to svn:ignore
    cmd = "svn propget svn:ignore \"%s\"" % local_img_dir
    ignore = subprocess.check_output(cmd, shell = True)
    for x in ignore.split("\n"):
        print "pre-ign:<%s>" % x
    ignore += os.path.basename(path)
    for x in ignore.split("\n"):
        print "post-ign:<%s>" % x
    cmd = "svn propset svn:ignore \"%s\" \"%s\"" % (ignore, local_img_dir)
    ignore = subprocess.check_output(cmd, shell = True)

    # commit the result
    # I'd really like to do this atomically
    # but I guess it's not possible?
    svn_commit("add image <%s>" % path, [placeholder_path, local_img_dir])
    # and finally commit the image data
    svn_commit("add image data <%s>" % path, img_path)

    # Make img file read only, since we wan't
    # to avoid accidental local overwrites since we
    # now have a symlink pointing to it.
    # This is not fool-proof and there should also
    # be a commit hook preventing this from happening
    # on the main server image repo.
    os.chmod(img_path, stat.S_IRUSR | stat.S_IROTH | stat.S_IRGRP)


def set_repo(repo):
    print 'Set repo to \"%s\".' % repo

def add_images(o, images):
    for image in images:
        print 'Add image \"%s\"' % image
        create_img(o, image)

def replace_image(o):
    print o.replace
    print o.src
    
    

def update_links():
        print 'Update links.'

def usage(argv):
    print "usage: %s --repo=<repo-root> --add=<path-to-image> --update-links[=<directory-to-update>" % argv[0]
    sys.exit()

def main():
    parser = optparse.OptionParser(usage = 'images.py [options]')
    parser.add_option(
        '--repo',
        dest = 'repo',
        action = 'store',
        help = 'Set the path to the image repository.')
    parser.add_option(
        '--add',
        dest = 'add',
        action = 'append',
        help = 'Add image to image repository.'
        )
    parser.add_option(
        '--replace',
        dest = 'replace',
        action = 'store',
        help = 'Replace image.'
        )

    parser.add_option(
        '--src',
        dest = 'src',
        action = 'store',
        help = 'Replace image.'
        )

    parser.add_option(
        '--update',
        dest = 'update',
        action = 'store_true',
        help = 'Update image links.')

    (o, args) = parser.parse_args()

#    if not args:
#        usage(sys.argv)

    print "o:", o
    print "args:", args
#    sys.exit(1)

    if len(args) > 0:
        sys.stderr.write('Error: Unexpected non-option arguments.\n')
        sys.exit(1)

    if not o.repo:
        sys.stderr.write('Error: No repo specified.\n')
        sys.exit(1)

    if not (o.repo or o.add or o.update):
        sys.stdout.write('Nothing to do.\n')
        sys.exit(0)

#    if o.repo:
#        set_repo(o.repo[0])
    if o.add:
        add_images(o, o.add)

    if o.replace:
        replace_image(o)
    if o.update:
        update_links()

if __name__ == '__main__':
#    test()
    main()
