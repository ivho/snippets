#!/usr/bin/env python

import sys, optparse
import subprocess
import os
import shutil
import stat
import tempfile

#FIXME: should we use md5sum which is available in mini-python in the
#       simics installation?
import sha

#TODO:
# - more pre-error checking (and usage)
# - Annotation for images?
# - git-svn support (at least for update)
# - test on windows
# - cleanup the svn:ignore handling - join add/remove code?
# - add a "verify/fixup/sync" option for svn:ignore (would be useful
#   when merging with conflicts)

IMG_URL="file:///space/work/simics/img_test/src_repo/images"
#IMG_URL="http://ala-svn.wrs.com/svn/Simics/simics/images"

PLACEHOLDER_SUFFIX = ".simimg"
HASH_LEN=40

def log(msg):
    print "=== ", msg
    pass

def cmd(cmd):
    log(" *** command <%s>" % cmd)
    x=subprocess.check_output(cmd, shell = True, stderr=subprocess.STDOUT)
    log("  == returns: <<%s>>" % x)
    return x

def svn_add_file(path):
    return cmd("svn add \"%s\"" % path)

def svn_rm_file(path):
    return cmd("svn rm \"%s\"" % path)

def svn_mkdir(dirname):
    return cmd("svn mkdir \"%s\"" % dirname)

def svn_commit(msg, paths):
    if type(paths) == type([]):
        args = " ".join(["\""+x+"\"" for x in paths])
    else:
        args = paths
    svncmd = "svn commit -m \"%s\" --depth empty %s" % (msg, args)
    for p in paths:
        cmd("svn up %s" % p)
    return cmd(svncmd)

def is_svn_file(path):
    try:
        x=cmd("svn ls \"%s\"" % path)
        return True
    except subprocess.CalledProcessError:
        return False
    return False

def is_symlink(path):
    return os.path.islink(path) and not is_svn_file(path)

def get_hash(path):
    log("get hash for <%s>" % path)
    s = sha.new()
    s.update(file(path).read())
    return s.hexdigest()

def error(msg):
# For debugging it might be nice to have all output on
# stdout so log/error doesn't interleave with.
    sys.stderr.write(msg)
#    log(msg)
    sys.exit(1)

def get_img_hash_path(path):
    # Get the hash of the file used for archiving
    # and split it into git-like structure.
    # xx/yyyyyyyyyyy
    hash = get_hash(path)
    dirname = hash[:2]
    fname = hash[2:]
    return (dirname, fname, hash)

def create_img(o, path, src_img_path = None, replace = False):

    img_added = False

    if not src_img_path:
        src_img_path = path

    # Check for errors as early as possible
    # to avoid an unclean svn status as much as possible
    # in case we need to abort.
    placeholder_path = path+PLACEHOLDER_SUFFIX
    if not replace and os.path.exists(placeholder_path):
        error("FATAL ERROR: %s already exists.\n" % placeholder_path)

    (dirname, fname, hash) = get_img_hash_path(src_img_path)

    # Create the file in img repo wisth hash based name
    img_dir = os.path.join(o.repo, dirname)
    img_path = os.path.join(img_dir, fname)
    img_commit = [] # remember whats been added until it's time to commit
    cmd("svn up --depth empty \"%s\"" % img_dir)
    if not os.path.exists(img_dir):
        svn_mkdir(img_dir)
        img_commit.append(img_dir)

    cmd("svn up --depth empty \"%s\"" % img_path)
    if not os.path.exists(img_path):
        log("cp %s -> %s" % (src_img_path, img_path))
        shutil.copy(src_img_path, img_path)
        svn_add_file(img_path)
        img_commit.append(img_path)
    # Make img file read only, since we wan't
    # to avoid accidental local overwrites since we
    # now have a symlink pointing to it.
    # This is not fool-proof and there should also
    # be a commit hook preventing this from happening
    # on the main server image repo.
        os.chmod(img_path, stat.S_IRUSR | stat.S_IROTH | stat.S_IRGRP)
    else:
        log("Image alread available in img_repo")

    # remove original so we can create the symlink
    # (already removed if this is --fix)
    if os.path.exists(path):
        log("remove original path <%s>" % path)
        os.remove(path)

    # Create the placeholder file containg the hash
    log("create place holder")
    hash_placeholder = file(placeholder_path, "w")
    hash_placeholder.write(hash)
    hash_placeholder.close()
    if not replace:
        svn_add_file(placeholder_path)


    # Create the local symlink pointing to the img_repo version
    # FIXME: Need a WIN32 version of this!!!!
    log("symlink: %s %s" % (img_path, path))
    os.symlink(img_path, path)
    local_img_dir = os.path.dirname(path)

    # Add the symlink to svn:ignore
    do_ignore = True
    icmd = "svn propget --strict svn:ignore \"%s\"" % local_img_dir
    ignore = subprocess.check_output(icmd, shell = True)

    # check for special case "empty" svn:ignore property
    if ignore == "\n":
        log("remove leading newline for previously empty svn:ignore")
        ignore = ""

    log("ignore<%s>" % ignore)
    fname = os.path.basename(path)
    for x in ignore.strip().split("\n"):
        log("checking <%s> <%s>" % (x, fname))
        if x == fname:
            log("already ignored <%s> == <%s>." % (x, fname))
            do_ignore = False
            break

    if do_ignore:
        ignore += fname
        for x in ignore.strip().split("\n"):
            log("post-ign:<%s>" % x)
        tmp = tempfile.NamedTemporaryFile(delete = False)
        tmp.write(ignore)
        tmp.close()
        icmd = "svn propset -F %s svn:ignore  \"%s\"" % (tmp.name, local_img_dir)
        ignore = subprocess.check_output(icmd, shell = True)

    # commit the result (i.e the placeholder and the svn:ignore property
    # changes on the directory and possibly the image data)
    svn_commit("Add image reference for <%s>." % path,
               [placeholder_path, local_img_dir])
    # and finally commit the image data
    if len(img_commit) != 0:
        # I'd really like to do this atomically with the previous commit
        # but I guess it's not possible since it's a different
        # checkout directory.
        svn_commit("add image data <%s>" % path, img_commit)

def add_images(o):
    for image in o.add:
        if is_svn_file(image):
            error("%s is a svn controlled file, use --fix\n" % image)
        log('Add image \"%s\"' % image)
        create_img(o, image)

# replace image for an image that's already been
# added with this script
def replace_image(o):
    create_img(o, o.replace, o.src, replace = True)

def svn_info(o, path):
    info = cmd("svn info \"%s\"" % path)
    d={}
    for i in info.strip().split("\n"):
        (tag, data) = i.split(":", 1)
        d[tag] = data.strip()
    return d

# Intended for the initial extermination of the images
# present in the SVN repo.
def fix_old_image(o):
    log("fix old image:")

    if not is_svn_file(o.fix):
        error("%s is not a subversion controllerd file.\n" % o.fix)

    info = svn_info(o, o.fix)
    (dirname, fname, hash) = get_img_hash_path(o.fix)

    # Check if we need to create the top hash dir in images/
    imgdir_url = IMG_URL + "/" + dirname
    try:
        r = svn_info(o, imgdir_url)
    except subprocess.CalledProcessError,e:
        cmd("svn mkdir -m \"create image directory <%s>\" \"%s\"" % (dirname, imgdir_url))

    mvcmd = "svn mv -m \"move image data for %s to image storage area.\"  \"%s\" \"%s/%s/%s\"" % (o.fix, info["URL"], IMG_URL, dirname, fname)
    cmd(mvcmd)

    # Now it's just a normal create_img()
    create_img(o, o.fix)

#FIXME: There is some code duplication with create_image
#       here, regarding svn:ignore
def remove_image(o):
    for path in o.remove:
        if not is_svn_file(path+PLACEHOLDER_SUFFIX):
            errror("Placeholder <%s> does not exist.\n")

        local_img_dir = os.path.dirname(path)
        fname = os.path.basename(path)
        icmd = "svn propget --strict svn:ignore \"%s\"" % local_img_dir
        ignore = subprocess.check_output(icmd, shell = True)

        # create a tmp file for svn:ignore update
        found_ignore = False
        tmp = tempfile.NamedTemporaryFile(delete = False)
        log(tmp.name)
        log(ignore.strip().split("\n"))
        empty = True
        for ifname in ignore.strip().split("\n"):
#            if ifname == "":
#                error("internal error: found empty line (remove 1)\n")
            if fname == ifname:
                found_ignore = True
                log("found <%s> in svn:ignore" % fname)
            else:
                empty = False
                log("writing <%s> to <%s>" % (ifname, tmp.name))
                tmp.write(ifname+"\n")

        if not found_ignore:
            error("Unable to find file <%s> in svn:ignore for directory <%s>\n" %
                  (fname, local_img_dir))
        tmp.close()

        if empty:
#FIXME: there is a choice here to either delete the svn:ignore property
#       or set it to empty (""), to avoid tree conflicts when merging.
#       I've chosen to set it to "". That way you can at least diff
#       the property changes on conflict.
#            icmd = "svn propdel svn:ignore  \"%s\"" % (local_img_dir)
            icmd = "svn propset svn:ignore \"\" \"%s\"" % (local_img_dir)
            ignore = subprocess.check_output(icmd, shell = True)
        else:
            icmd = "svn propset -F %s svn:ignore  \"%s\"" % (tmp.name, local_img_dir)
            ignore = subprocess.check_output(icmd, shell = True)

        if is_symlink(path):
            os.remove(path)

        svn_rm_file(path+PLACEHOLDER_SUFFIX)
        svn_commit("remove image reference for <%s>" % path,
                   [path+PLACEHOLDER_SUFFIX, local_img_dir])

def update_link(o, dirname, fname):
    path=os.path.join(dirname, fname)
    linkpath = path[:len(path)-len(PLACEHOLDER_SUFFIX)]

    hash=file(path).read()
    if len(hash) != HASH_LEN:
        error("%s does not contain valid hash." % path)

    if not is_svn_file(path):
        error("%s is not a subversion controlled file.\n" % path)

    img_dir = os.path.join(o.repo, hash[:2])
    img_path = os.path.join(img_dir, hash[2:])

    if not is_svn_file(img_path):
        log("updating for %s" % img_path)
        cmd("svn up --depth empty \"%s\"" % img_dir)
        cmd("svn up --depth empty \"%s\"" % img_path)
        if not is_svn_file(img_path):
            error("Failed to find image data for %s\n hash:%s\nimg_path:%s\n" %
                  (linkpath, hash, img_path))

    if os.path.exists(linkpath):
        error("%s already exists, and is not a symlink.\n")
    os.symlink(img_path, linkpath)


def update_links(o):
    for svndir in o.update:
        for (path, dirs, files) in os.walk(svndir):
            if '.svn' in dirs:
                dirs.remove('.svn')
            # First, find all symlinks and remove them
            for f in files:
                fp = os.path.join(path, f)
                if is_symlink(fp):
                    os.remove(fp)
            # Second, re-create symlinks for all .simimg files
            for f in files:
                if f.endswith(PLACEHOLDER_SUFFIX):
                    update_link(o, path, f)

def setup(o):
    if os.path.exists(o.repo):
        error("%s already exists, can't initialize a new image repo there." %
              o.repo)
    cmd("svn co --depth empty \"%s\" \"%s\"" % (IMG_URL, o.repo))

def main():
    parser = optparse.OptionParser(usage = 'images.py [options]')
    parser.add_option(
        '--repo',
        dest = 'repo',
        action = 'store',
        metavar = '<img-repo-dir>',
        help = 'Set the path to the image repository.')

    parser.add_option(
        '--setup',
        dest = 'setup',
        action = 'store_true',
        help = 'Initialize and setup a new empty image reop in <img-repo-dir>.')

    parser.add_option(
        '--add',
        dest = 'add',
        action = 'append',
        metavar = '<unversioned-image-file>',
        help = 'Add image to image repository and a reference in src repo.'
        )
    parser.add_option(
        '--remove',
        dest = 'remove',
        action = 'append',
        metavar = '<versioned-image-file>',
        help = 'Remove reference to image in src repo.'
        )

    parser.add_option(
        '--replace',
        dest = 'replace',
        action = 'store',
        metavar = '<versioned-image-file>',
        help = 'Replace image.'
        )

    parser.add_option(
        '--src',
        dest = 'src',
        action = 'store',
        metavar = '<unversioned-image-file>',
        help = 'New image to replace.'
        )

    parser.add_option(
        '--update',
        dest = 'update',
        action = 'append',
        metavar = '<directoy>',
        help = 'Update image links in directory.')

    parser.add_option(
        '--fix',
        dest = 'fix',
        action = 'store',
        metavar = '<old-svn-image>',
        help = 'Fix existing image. Should only be used during initial repo conversion.')

    (o, args) = parser.parse_args()

    if len(args) > 0:
        sys.stderr.write('Error: Unexpected non-option arguments.\n')
        sys.exit(1)

    if not o.repo:
        sys.stderr.write('Error: No repo specified.\n')
        sys.exit(1)

    if not (o.repo or o.add or o.update):
        sys.stdout.write('Nothing to do.\n')
        sys.exit(0)

    if o.setup:
        setup(o)

    if o.fix:
        fix_old_image(o)

    if o.add:
        add_images(o)

    if o.replace:
        replace_image(o)

    if o.remove:
        remove_image(o)

    if o.update:
        update_links(o)

# helper to find some files that generate collision in the
# directory part of the hash for the test system.
def findsha(dirname, instance = 0):
    y = 0
    i = 0
    while True:
        x=file("/tmp/kalle", "w")
        x.write("abcd%d" % y)
        x.close()
        z = get_hash("/tmp/kalle")
        if (z[:2] == dirname):
            if i == instance:
                print("/tmp/kalle (abcd%d) is now sha1<%s>" % (y, z))
                return
            i += 1
        y += 1

if __name__ == '__main__':
#    test()
    main()
#    findsha("95")
#    findsha("95", instance = 1)


