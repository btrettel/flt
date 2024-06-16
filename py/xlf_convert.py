#!/usr/bin/env -S python3 -Werror
# -*- coding: utf-8 -*-

import os
import argparse

parser = argparse.ArgumentParser(description="Converts all files to a form that should be compatible with IBM XL Fortran, and pass all tests.")
parser.add_argument("--undo", action="store_true", help="undo changes", default=False)
args = parser.parse_args()

directories = ["app", "src", "test"]

filepaths = []
for directory in sorted(directories):
    # Canonicalize the filepath, so that (for example), this works if `.\` is in front of the path, like PowerShell does.
    directory = os.path.relpath(directory)
    
    if not os.path.isdir(directory):
        print("{} is not a directory.".format(directory))
        fail = True
    else:
        for filename in os.listdir(directory):
            if filename.endswith(".f90"):
                filepaths.append(os.path.join(directory, filename))

assert(len(filepaths) > 0)

# TODO: `error stop` doesn't allow for character codes.
for filename in sorted(filepaths):
    with open(filename, "r") as file_handler:
        for line in file_handler.readlines():
            if line.strip().startswith("error stop"):
                print(filename)
            

# TODO: Disable `! Test assertion failure message.`
