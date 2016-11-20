#!/usr/bin/python
#-*- coding: utf-8 -*-

# generates diagmsg.h from diagmsg.txt.

import os
import sys
import re

class diagobj():
    def __init__(self,name="",msg=""):
        self.name = name
        self.msg = msg

    def __str__(self):
        return self.name + ": " + self.msg


def gen_diagmsg_h(objs):
    with open("diagmsg.h", 'w') as f:
        f.write("// Don't touch this file.\n")
        f.write("#ifndef DIAGMSG_H\n")
        f.write("#define DIAGMSG_H\n")
        for obj in objs:
            f.write("#define " + obj.name + "  \\\n")
            f.write("\t" + obj.msg + "\n")
        f.write("#endif /* DIAGMSG_H */\n")


def parse_diagmsg(c):
    # non-blank lines
    lines = [l for l in c.splitlines() if l.strip()]
    # filter-out comment lines
    lines = [l for l in lines if not l.startswith('#')]
    objs = []

    for i in xrange(0, len(lines), 2):
        # name: xxx
        result = re.search("\s*name\s*:\s*([A-Za-z_0-9]+)", lines[i])
        name = result.group(1)
        # msg: "yyy"
        result = re.search("\s*msg\s*:\s*(\".*\")", lines[i+1])
        msg = result.group(1)
        obj = diagobj(name, msg)
        objs.append(obj)

    gen_diagmsg_h(objs)


def main():
    if (len(sys.argv) < 2):
        sys.exit("Usage: " + os.path.basename(sys.argv[0]) + " <file>")
    with open(sys.argv[1], 'r') as f:
        c = f.read()
        parse_diagmsg(c)


if __name__ == '__main__':
    main()
