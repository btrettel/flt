#!/usr/bin/env -S python3 -Werror
# -*- coding: utf-8 -*-

import os
from subprocess import run, PIPE

with open(os.path.join("src", "revision.f90"), "w") as output_handler:
    output_handler.write("character(len=*), public, parameter :: REVISION = \"")
    result = run(["svn", "info", "--show-item", "revision"], stdout=PIPE, stderr=PIPE, universal_newlines=True)
    output_handler.write(result.stdout.strip())
    output_handler.write("\"\n")
    
    output_handler.write("character(len=*), public, parameter :: REVISION_DATE = \"")
    result = run(["svn", "info", "--show-item", "last-changed-date"], stdout=PIPE, stderr=PIPE, universal_newlines=True)
    output_handler.write(result.stdout.strip())
    output_handler.write("\"\n")
    
    output_handler.write("logical, public, parameter :: MODIFIED = ")
    result = run(["svn", "diff", "--summarize"], stdout=PIPE, stderr=PIPE, universal_newlines=True)
    if len(result.stdout.strip()) > 0:
        output_handler.write(".true.\n")
    else:
        output_handler.write(".false.\n")
    
    output_handler.write("character(len=*), public, parameter :: TAG = REVISION")
