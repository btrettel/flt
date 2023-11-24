#!/usr/bin/env -S python3 -Werror
# -*- coding: utf-8 -*-

# # $File$
# 
# Summary: 
# Author: Ben Trettel (<http://trettel.us/>)
# Last updated: $Date$
# Revision: $Revision$
# Project: [flt](https://github.com/btrettel/flt)
# License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

import argparse
import json

parser = argparse.ArgumentParser(description="checks whether flt tests passed based on a JSONL output file")
parser.add_argument("file", help="JSONL file to read")
parser.add_argument("--failure_expected", action="store_true", help="use this flag if the tests are expected to fail", default=False)
args = parser.parse_args()

with open(args.file) as jsonl_file_handler:
    line = jsonl_file_handler.readline()
    
    first_time_set = False
    
    while line:
        data = json.loads(line)
        
        last_rc      = data["rc"]
        last_message = data["message"]
        
        if "total tests" in data.keys():
            number_of_tests = data["total tests"]
        
        if "total failures" in data.keys():
            number_of_failures = data["total failures"]
        
        # Advance line
        line = jsonl_file_handler.readline()

if not args.failure_expected:
    assert last_rc == 0
    assert last_message == "All tests passed."
    assert number_of_tests > 0
    assert number_of_failures == 0
else:
    assert last_rc != 0
    assert last_message != "All tests passed."
    assert number_of_tests > 0
    assert number_of_failures > 0
