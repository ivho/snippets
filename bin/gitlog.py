#!/usr/bin/python
import sys
import datetime
import subprocess

def git_cmd(gitdir, cmd, opts):
        cmdlist = ["git",
                   "--git-dir=%s/.git" % gitdir,
                   cmd] + opts

        x=subprocess.Popen(cmdlist,
                           stderr=subprocess.PIPE,
                           stdout=subprocess.PIPE)
        (o,e)=x.communicate()
        if x.returncode != 0:
            print e
            raise Exception("GIT ERROR")
        return o

class GitCommit(object):
    def __init__(self, gitdir, gitall, sha, cdate, adate, author, msg):
        self.sha = sha
        self.gitall = gitall
        self.cdate = datetime.datetime.fromtimestamp(cdate)
        self.adate = datetime.datetime.fromtimestamp(adate)
        self.author = author
        self.msg = msg
        # Keep git gitdir so we can fetch more complex
        # info (with git_cmd) on specific commits later if needed.
        self.gitdir = gitdir

    def get_files(self):
        return filter(lambda a:a!="",
                      git_cmd(self.gitdir,
                              "diff-tree",
                              ["--no-commit-id",
                               "--name-only",
                               "-r", self.sha]).split("\n"))
    def get_branches(self):
        res = git_cmd(self.gitdir,
                      "branch",
                      ["--list",
                       "--all",
                       "--contains",
                       self.sha])
        return "|".join(res.split("\n"))

    def __repr__(self):
        return "%s <%s> %s %s" % (self.cdate,
                                  self.author,
                                  self.sha[:8],
                                  self.msg.strip())

class GitActivity(object):
    def __init__(self, gitdir, gitall, reponame = None):
        self.commits = []
        self.gitdir = gitdir
        self.gitall = ["--all"] if gitall else []
        self.user_email = git_cmd(gitdir, "config",
                                  ["--get","user.email"]
                                  ).strip()
        self.user_name = git_cmd(gitdir, "config",
                                  ["--get","user.name"]
                                  ).strip()

        o = git_cmd(gitdir, "log", self.gitall +
                    ["--after=2013-06-06",
                     "--format=format:%ct:%at:%H:%ae:%s"])
        for l in o.split("\n"):
            (ct,at,sha,author,msg) = l.split(":", 4)
            ge = GitCommit(sha = sha,
                           adate = int(at),
                           cdate = int(ct),
                           author = author,
                           msg = msg,
                           gitdir = gitdir,
                           gitall = self.gitall)
            self.commits.append(ge)

if __name__ == "__main__":
    ga = GitActivity(sys.argv[1], True)
    for a in ga.commits:
        print a
