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

def cmd(cmd):
    log(" === command <%s>" % cmd)
    r = subprocess.call(cmd, shell = True)
    if r != 0:
        log("cmd <%s> failed" % cmd)
    return r

def svn_add_file(path):
    return cmd("svn add %s" % path)

def svn_mkdir(dirname):
    return cmd("svn mkdir %s" % dirname)

def svn_commit(msg, paths):
    if type(paths) == type([]):
        args = " ".join(paths)
    else:
        args = paths
    svncmd = "svn commit -m \"%s\" --depth empty %s" % (msg, args)
    for p in paths:
        cmd("svn up %s" % p)
#    print "skipping commit cmd <%s>" % svncmd
    return cmd(svncmd)

def get_hash(path):
    log("get hash for <%s>" % path)
    s = sha.new()
    s.update(file(path).read())
    return s.hexdigest()

def create_img(o, path, src_img_path = None, ):
    if not src_img_path:
        src_img_path = path

    # Check for errors as early as possible
    # to avoid an unclean svn status as much as possible
    # in case we need to abort.
    placeholder_path = path+PLACEHOLDER_SUFFIX
    if os.path.exists(placeholder_path):
        print "FATAL ERROR: %s already exists." % placeholder_path
        sys.exit(1)


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
    log("move %s -> %s" % (path, src_img_path))
    shutil.move(src_img_path, img_path)
    svn_add_file(img_path)

    # Create the placeholder file containg the hash

    hash_placeholder = file(placeholder_path, "w")
    hash_placeholder.write(hash)
    hash_placeholder.close()
    svn_add_file(placeholder_path)

    # Create the local symlink pointing to the img_repo version
    # FIXME: Need a WIN32 version of this!!!!
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
    svn_commit("add image data <%s>" % path, [img_dir, img_path])

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

# replace image for an image that's already been
# added with this script
def replace_image(o):
    print o.replace
    print o.src
    add

# Intended for the initial extermination of the images
# present in the SVN repo.
def fix_old_image(o):
    log("fix old image:")

    # Find a free tmp name for this file in the same dir
    tmp = 0
    while True:
        tmpnam = "%s.%d" % (o.fix, tmp)
        if not os.path.exists(tmpnam):
            break
        tmp += 1

    # make a copy of the image and remove the svn version
    cmd("cp %s %s" % (o.fix, tmpnam))
    cmd("svn rm %s" % o.fix)
    svn_commit("fix old svn image <%s>" % o.fix, [o.fix, os.path.dirname(o.fix)])

    # Now it's just a normal create_img()
    create_img(o, o.fix, tmpnam)

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

    parser.add_option(
        '--fix',
        dest = 'fix',
        action = 'store',
        help = 'Fix existing image.')

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
    if o.fix:
        fix_old_image(o)

    if o.add:
        add_images(o, o.add)

    if o.replace:
        replace_image(o)
    if o.update:
        update_links()

if __name__ == '__main__':
#    test()
    main()
