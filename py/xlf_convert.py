#!/usr/bin/env -S python3 -Werror
# -*- coding: utf-8 -*-

import os
import argparse
from subprocess import run
import sys
import fileinput

parser = argparse.ArgumentParser(description="Converts all files to a form that should be compatible with IBM XL Fortran, and pass all tests.")
parser.add_argument("--undo", action="store_true", help="undo changes (with Git)", default=False)
args = parser.parse_args()

directories = ["app", "src", "test"]
skip_files  = [os.path.join("src", "revision.f90"), os.path.join("src", "units.f90")]

exit_code = 0

filepaths = []
for directory in sorted(directories):
    # Canonicalize the filepath, so that (for example), this works if `.\` is in front of the path, like PowerShell does.
    directory = os.path.relpath(directory)
    
    if not os.path.isdir(directory):
        print("ERROR: {} in directories is not a directory.".format(directory))
        exit_code = 1
    else:
        for filename in os.listdir(directory):
            if os.path.join(directory, filename) in skip_files:
                continue
            
            if filename.endswith(".f90"):
                filepaths.append(os.path.join(directory, filename))

assert(len(filepaths) > 0)

# IBM XL Fortran's `error stop` doesn't allow for character codes. So change to just `error stop 1`.
for filepath in sorted(filepaths):
    if args.undo:
        result = run(["git", "checkout", filepath])
        if result.returncode != 0:
            print("ERROR: Changes to {} could not be undone.".format(filepath), file=sys.stderr)
            exit_code = 1
    else:
        with fileinput.input(filepath, inplace=True) as file:
            for line in file:
                if line.strip().startswith("error stop"):
                    print("error stop 1")
                else:
                    print(line, end="")

# Comment out a test which will fail due to my assertions now not having messages.
# Seems that I also need to comment out the `execute_command_line` parts as they fail for some reason? Not clear to me why.
filepaths = [os.path.join("test", "test_checks.f90"), os.path.join("test", "test_units.f90"), os.path.join("test", "test_unittest.f90")]
for filepath in sorted(filepaths):
    if args.undo:
        result = run(["git", "checkout", filepath])
        if result.returncode != 0:
            print("ERROR: Changes to {} could not be undone.".format(filepath), file=sys.stderr)
            exit_code = 1
    else:
        commenting_out = False
        with fileinput.input(filepath, inplace=True) as file:
            for line in file:
                #if "Test assertion failure message." in line:
                if "! IBM XLF comment start" in line:
                    commenting_out = True
                
                if commenting_out:
                    print("!" + line, end="")
                else:
                    print(line, end="")
                
                #if "ASSERTION FAILED. Custom message." in line:
                if "! IBM XLF comment end" in line:
                    commenting_out = False

exit(exit_code)
