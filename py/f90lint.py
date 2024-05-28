#!/usr/bin/env -S python3 -Werror
# -*- coding: utf-8 -*-

import os
import argparse
import configparser

ASSERTION_DENSITY_LOWER_LIMIT = 2.0
TEST_RATIO_LOWER_LIMIT        = 0.5

class file_stats_class:
    def __init__(self, filename, assertion_density, lines):
        self.filename          = filename
        self.assertion_density = assertion_density
        self.lines             = lines
        self.test_ratio        = 0.0

def is_empty_or_comment(line):
    line_clean = line.split("!")[0].strip()
    
    return not len(line_clean) > 0

def canonicalize_path(path):
    path_split = path.split("/")
    return os.path.join(*path_split)

def canonicalize_paths(path_string):
    path_string_split = path_string.split(' ')
    paths = []
    for path_string_split_i in path_string_split:
        paths.append(canonicalize_path(path_string_split_i))
    return paths

parser = argparse.ArgumentParser(description="A linter for Fortran 90 and later intended for Ben Trettel's use.")
parser.add_argument("file", help="input file to read")
args = parser.parse_args()

config = configparser.ConfigParser()
config.read(args.file)

directories              = config['f90lint']['directories'].split(' ')
skip_indexing            = canonicalize_paths(config['f90lint']['skip_indexing'])
ignore_assertion_density = canonicalize_paths(config['f90lint']['ignore_assertion_density'])
ignore_test_sloc_ratio   = canonicalize_paths(config['f90lint']['ignore_test_sloc_ratio'])

fail = False

filepaths = []
for directory in sorted(directories):
    # Canonicalize the filepath, so that (for example), this works if `.\` is in front of the path, like PowerShell does.
    directory = os.path.relpath(directory)
    
    if not os.path.isdir(directory):
        print("{} is not a directory.")
        fail = True
    else:
        for filename in os.listdir(directory):
            if filename.endswith(".f90"):
                filepaths.append(os.path.join(directory, filename))

if fail:
    print("Error(s) encountered, stopping.")
    exit(1)

exit_code = 0

global_num_lines_code  = 0
global_num_lines_tests = 0
global_num_assertions  = 0
file_stats = []
for filename in sorted(filepaths):
    if filename in skip_indexing:
        continue
    
    with open(filename, "r") as file_handler:
        contains_executable_code = False
        for line in file_handler.readlines():
            if line.strip().startswith("program ") or line.strip().startswith("contains"):
                contains_executable_code = True
                break
        
        # Ignore files that don't contain executable code.
        if contains_executable_code:
            file_handler.seek(0)
            local_num_lines_code_or_tests = 0
            local_num_lines_tests         = 0
            local_num_assertions          = 0
            for line in file_handler.readlines():
                # If the line isn't empty or only comments.
                if not is_empty_or_comment(line):
                    local_num_lines_code_or_tests = local_num_lines_code_or_tests + 1
                    
                    # Tests don't have assertions, so they shouldn't count towards the assertion count.
                    if not filename.startswith("test"):
                        global_num_lines_code = global_num_lines_code + 1
                        
                        # `error stop` lines are for error termination, so they are like assertions. They are used where assertions aren't appropriate.
                        if (line.strip().startswith("call assert") or line.strip().startswith("error stop") or ("%check(" in line)):
                            global_num_assertions = global_num_assertions + 1
                            local_num_assertions  = local_num_assertions + 1
                    else:
                        global_num_lines_tests = global_num_lines_tests + 1
            
            file_stats.append(file_stats_class(filename, 100.0 * local_num_assertions / local_num_lines_code_or_tests, local_num_lines_code_or_tests))

# Calculate `test_ratio`s.
for file_stat in file_stats:
    if not file_stat.filename.startswith("test"):
        for sub_file_stat in file_stats:
            if sub_file_stat.filename.startswith("test"):
                if os.path.basename(sub_file_stat.filename) == "test_" + os.path.basename(file_stat.filename):
                    file_stat.test_ratio = sub_file_stat.lines / file_stat.lines
                    #print(file_stat.filename, sub_file_stat.filename, file_stat.test_ratio)
                    break

for file_stat in file_stats:
    if (file_stat.assertion_density < ASSERTION_DENSITY_LOWER_LIMIT) and (not file_stat.filename.startswith("test")) and (not file_stat.filename in ignore_assertion_density):
        print("{}: Assertion density is {:.2f}% (<{:.2f}%).".format(file_stat.filename, file_stat.assertion_density, ASSERTION_DENSITY_LOWER_LIMIT))
        exit_code = 1
    
    if (file_stat.test_ratio < TEST_RATIO_LOWER_LIMIT) and file_stat.filename.startswith("src") and (not file_stat.filename in ignore_test_sloc_ratio):
        print("{}: Lines-of-tests to lines-of-code ratio is {:.2f} (<{:.2f}).".format(file_stat.filename, file_stat.test_ratio, TEST_RATIO_LOWER_LIMIT))
        exit_code = 1

global_assertion_density = 100.0 * global_num_assertions / global_num_lines_code
if (global_assertion_density < ASSERTION_DENSITY_LOWER_LIMIT):
    print("Global assertion density is {:.2f}% (<{:.2f}%).".format(global_assertion_density, ASSERTION_DENSITY_LOWER_LIMIT))
    exit_code = 1

global_test_ratio = global_num_lines_tests / global_num_lines_code
if (global_test_ratio < TEST_RATIO_LOWER_LIMIT):
    print("Global lines-of-tests to lines-of-code ratio is {:.2f} (<{:.2f}).".format(global_test_ratio, TEST_RATIO_LOWER_LIMIT))
    exit_code = 1

print("\n==============")
print("= Statistics =")
print("==============")
print("Global assertion density: {:.2f}%".format(global_assertion_density))
print("Global lines-of-tests to lines-of-code ratio: {:.2f}".format(global_test_ratio))

exit(exit_code)
